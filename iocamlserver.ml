(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: HTTP server + Kernel control
 *
 *)

(*

kernel messages
---------------

 ipython/html/services/kernels/handlers.py
 ipython/html/static/notebook/js/notebook.js 

 /kernels?notebook=<guid> - send kernel id + ws_url, start kernel
 /kernels/<guid> - delete, stop kernel, status 204
 /kernels/<guid>/restart|interrupt - restart/interrupt kernel
 /kernels/<guid>/shell|iopub|stdin - websocket

notebook messages
-----------------

 ipython/html/services/notebooks/handlers.py

 /notebooks
 /notebooks/<guid>
 /notebooks/<guid>/checkpoints
 /notebooks/<guid>/checkpoints/<id>


root messages 
-------------

 ipython/html/notebook/handlers.py

 /<guid>
 /new - new notebook
 /<guid>/copy - copy and redirect
 /<name> - redirect to guid

*)

open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix

(* port configuration *)
let http_addr = "127.0.0.1"
let http_port = 8889
let ws_addr = "127.0.0.1"
let ws_port = 8890
let zmq_shell_port = 8891
let zmq_iopub_port = 8892
let zmq_control_port = 8893
let zmq_heartbeat_port = 8894
let zmq_stdin_port = 8895

let notebook_path = Unix.getcwd ()

(* zmq initialization *)
let zmq = ZMQ.init ()

(* the json that's served for an empty notebook *)
let empty_notebook title = 
    let open Yojson.Basic in
    to_string 
        (`Assoc [
            "metadata", `Assoc [ "language", `String "ocaml"; "name", `String title; ];
            "nbformat", `Int 3; "nbformat_minor", `Int 0;
            "worksheets", `List [ `Assoc [ "cells", `List []; "metadata", `Assoc []; ]; ];
        ])

let write_empty_notebook title = 
    Lwt_io.(with_file ~mode:output (title ^ ".ipynb") 
                (fun f -> write f (empty_notebook title)))

let kernel_id_json ~kernel_guid ~ws_addr ~ws_port = 
    let open Yojson.Basic in
    to_string 
        (`Assoc [
            "kernel_id", `String kernel_guid;
            "ws_url", `String ("ws://" ^ ws_addr ^ ":" ^ string_of_int ws_port);
        ])

let kernel_response req =
    (* I think at this point we create the kernel and add a mapping *)
    let kernel = Kernel.init_kernel
        ~zmq ~path:notebook_path ~ip_addr:ws_addr
        ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
        ~zmq_heartbeat_port ~zmq_stdin_port 
    in
    (*let () = 
        Kernel.g_notebooks := Kernel.KMap.add notebook_guid k.Kernel.guid !Kernel.g_notebooks;
        Kernel.g_kernels := Kernel.KMap.add kernel_guid k !Kernel.g_kernels
    in*)
    Server.respond_string ~status:`OK
        ~body:(kernel_id_json ~kernel_guid:kernel.Kernel.guid ~ws_addr ~ws_port) ()

let notebook_list () = 
    lwt l = Files.list_notebooks notebook_path in
    let open Yojson.Basic in
    let json nb =
        let nguid = Kernel.notebook_of_name nb in
       `Assoc [ 
            "kernel_id", 
                (match Kernel.kernel_of_notebook nguid with 
                 | None -> `Null 
                 | Some(x) -> `String x);
            "name", `String nb;
            "notebook_id", `String nguid;
       ]
    in
    let json = `List (List.map json l) in
    Server.respond_string ~status:`OK ~body:(to_string json) ()

let header typ = 
    let h = Header.init () in
    let h = Header.add h "Content-Type" typ in
    let h = Header.add h "Server" "iocaml" in
    h

let header_none = Header.init ()
let header_html = header "text/html; charset=UTF-8"
let header_css = header "text/css"
let header_javascript = header "application/javascript"
let header_json = header "application/json"
let header_font = header "application/x-font-woff"
let header_redirect guid = 
    let h = header_html in
    let h = Header.add h "Location" ("/" ^ guid) in
    h

let header_of_extension filename = 
    if Filename.check_suffix filename ".js" then header_javascript
    else if Filename.check_suffix filename ".css" then header_css
    else if Filename.check_suffix filename ".ipynb" then header_json
    else if Filename.check_suffix filename ".woff" then header_font
    else header_none

let make_server address port =
    let callback conn_id ?body req =
        let uri = Request.uri req in
        let meth = Request.meth req in
        let path = Uri.path uri in
        
        let not_found () = 
            Printf.eprintf "%s: ERROR: %s -> %s\n%!" 
                (Connection.to_string conn_id) (Uri.to_string uri) path;
            Server.respond_not_found ()
        in

        lwt decode = Uri_paths.decode path in
        let ()  = 
            (* XXX log all messages that are not just serving notebook files *)
            if decode <> `Static then 
                Printf.eprintf "%s: %s -> %s\n%!" 
                    (Connection.to_string conn_id) (Uri.to_string uri) path;
        in

        match decode with

        | `Root ->
            (*
            let notebook = Pages.generate_notebook_html 
                ~title:"Untitled0" ~path:notebook_path ~notebook_guid:Uuidm.(to_string (create `V4))
            in
            Server.respond_string ~status:`OK ~headers:header_html ~body:notebook ()
            *)
            let dashboard = Pages.generate_dashboard_html ~path:notebook_path in
            Server.respond_string ~status:`OK ~headers:header_html ~body:dashboard ()

        | `Static -> 
            let fname = Server.resolve_file ~docroot:"ipython/html" ~uri:(Request.uri req) in
            if Sys.file_exists fname then 
                Server.respond_file ~headers:(header_of_extension fname) ~fname ()
            else not_found ()

            (* experiment - serve from crunched file system 
            let fname = Server.resolve_file ~docroot:"" ~uri:(Request.uri req) in
            let fname = String.sub fname 7 (String.length fname-7) in (* strip static *)
            (match Crunched.read fname with
            | None -> not_found()
            | Some(x) -> 
                Server.respond_string ~status:`OK ~headers:(header_of_extension fname) ~body:x ())
            *)

        | `Root_guid(guid) -> not_found ()

        | `Root_new -> 
            (* create new .ipynb file *)
            lwt () = Lwt_io.eprintf "*** new\n" in
            lwt name = Files.(list_notebooks notebook_path >>= new_notebook_name) in
            lwt () = write_empty_notebook name in
            let guid = Kernel.notebook_of_name name in
            (* 302 Found, redirect to `Root_guid *)
            Server.respond_string ~status:`Found ~headers:(header_redirect guid) ~body:"" ()

        | `Root_copy(guid) -> not_found ()
        | `Root_name(guid) -> not_found ()

        | `Notebooks -> notebook_list ()

        | `Notebooks_guid(_) when meth = `GET -> 
            Server.respond_string ~status:`OK ~body:(empty_notebook "Untitled0") ()

        | `Notebooks_guid(_) when meth = `PUT -> 
            (* save notebook *)
            not_found ()

        | `Notebooks_checkpoint(_) -> 
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Notebooks_checkpoint_id(_) -> not_found ()

        | `Clusters ->
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Kernels -> 
            (*kernel_response req*)
            not_found ()

        | `Kernels_guid(_) -> not_found ()
        | `Kernels_restart(_) -> not_found ()
        | `Kernels_interrupt(_) -> not_found ()

        | `Error_not_found | _ -> not_found ()
    in
    let conn_closed conn_id () =
        Printf.eprintf "%s: closed\n%!" (Connection.to_string conn_id)
    in
    let config = { Server.callback; conn_closed } in
    Server.create ~address ~port config

let run_servers () = 
    let http_server = make_server http_addr http_port in
    let ws_server = 
        return 
            (Websocket.establish_server 
                (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string ws_addr, ws_port))
                Bridge.ws_init)
    in
    let rec wait_forever () = Lwt_unix.sleep 1000.0 >>= wait_forever in
    let ws_server = ws_server >>= fun _ -> wait_forever () in
    Lwt.join [ http_server; ws_server ]

let close_kernels () = 
    (* kill all running kernels *)
    Kernel.iter_kernels
        (fun _ v -> 
            eprintf "killing kernel: %s\n" v.Kernel.guid;
            v.Kernel.process#terminate) 

let _ = 
    Sys.catch_break true;
    try 
        (*at_exit close_kernels;*)
        Lwt_unix.run (run_servers ())
    with
    | Sys.Break -> begin
        close_kernels ();
        (*ZMQ.term zmq*)
    end


