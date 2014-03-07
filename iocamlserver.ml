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
let header_date h = 
    let day = function 
        | 0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed"
        | 4 -> "Thu" | 5 -> "Fri" | _ -> "Sat"
    in
    let month = function
        | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr"
        | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul" | 7 -> "Aug"
        | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | _ ->"Dec"
    in
    let tm = Unix.(gmtime (gettimeofday ())) in
    let tm = Unix.(Printf.sprintf "%s, %.2i %s %.4i %.2i:%.2i:%.2i GMT" 
        (day tm.tm_wday) tm.tm_mday (month tm.tm_mon) (tm.tm_year+1900) tm.tm_hour tm.tm_min tm.tm_sec)
    in
    let h = Header.add h "Date" tm in
    h

let checkpoint_date () = 
    let tm = Unix.(gmtime (gettimeofday ())) in
    Unix.(Printf.sprintf "%.4i-%.2i-%.2iT%.2i:%.2i:%.2i.000000+00:00" 
        (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday 
        tm.tm_hour tm.tm_min tm.tm_sec)

let header_of_extension filename = 
    if Filename.check_suffix filename ".js" then header_javascript
    else if Filename.check_suffix filename ".css" then header_css
    else if Filename.check_suffix filename ".ipynb" then header_json
    else if Filename.check_suffix filename ".woff" then header_font
    else header_none

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

let not_found () = 
    lwt () = Lwt_io.eprintf "Not_found\n" in
    Server.respond_not_found ()
 
let notebook_list () = 
    lwt l = Files.list_notebooks notebook_path in
    let open Yojson.Basic in
    let json nb =
        let notebook_guid = Kernel.M.notebook_guid_of_filename nb in
       `Assoc [ 
            "kernel_id", (* check if kernel is already running *)
                (match Kernel.M.kernel_of_notebook_guid notebook_guid with 
                 | None -> `Null 
                 | Some(x) -> `String (Kernel.M.kernel_guid_of_kernel x));
            "name", `String nb;
            "notebook_id", `String notebook_guid;
       ]
    in
    let json = `List (List.map json l) in
    Server.respond_string ~status:`OK ~body:(to_string json) ()

let find_dict name json = 
    match json with
    | `Assoc(l) -> ((wrap2 List.assoc) name l)
    | _ -> fail (Failure "find_dict")

let get_string = function
    | `String s -> return s 
    | _ -> fail (Failure "get_string")

(* extract filename from metadata *)
let get_filename_of_ipynb s = 
    Yojson.Basic.from_string s |> find_dict "metadata" >>= find_dict "name" >>= get_string

let save_notebook cur_guid body = 
    lwt filename = get_filename_of_ipynb body in
    let guid = Kernel.M.notebook_guid_of_filename filename in
    lwt () = Lwt_io.(with_file ~mode:output 
        (Kernel.M.filename_of_notebook_guid guid ^ ".ipynb")
        (fun f -> write f body))
    in
    (* what if cur_guid != guid ie a rename *)
    Server.respond_string ~status:`OK ~headers:(header_date header_html) ~body:"" ()

let http_server address port =
    let callback conn_id ?body req =
        let uri = Request.uri req in
        let meth = Request.meth req in
        let path = Uri.path uri in

        lwt decode = Uri_paths.decode path in
        let ()  = 
            (* XXX log all messages that are not just serving notebook files *)
            if decode <> `Static then 
                Printf.eprintf "%s: %s -> %s\n%!" 
                    (Connection.to_string conn_id) (Uri.to_string uri) path;
        in

        let query_param var = 
            match Uri.get_query_param uri var with
            | None -> Lwt.fail (Failure ("failed to get param: " ^ var))
            | Some(x) -> return x
        in

        match decode with

        | `Root ->
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

        | `Root_guid(guid) -> 
            let notebook = Pages.generate_notebook_html 
                ~title:"IOCaml-Notebook" ~path:notebook_path ~notebook_guid:guid
            in
            Server.respond_string ~status:`OK ~headers:header_html ~body:notebook ()

        | `Root_new -> 
            (* create new .ipynb file *)
            lwt name = Files.(list_notebooks notebook_path >>= new_notebook_name) in
            lwt () = Lwt_io.(with_file ~mode:output (name ^ ".ipynb") 
                (fun f -> write f (empty_notebook name)))
            in
            let guid = Kernel.M.notebook_guid_of_filename name in
            (* 302 Found, redirect to `Root_guid *)
            Server.respond_string ~status:`Found ~headers:(header_redirect guid) ~body:"" ()

        | `Root_copy(guid) -> not_found ()
        | `Root_name(guid) -> not_found ()

        | `Notebooks -> notebook_list ()

        | `Notebooks_guid(guid) when meth = `GET ->
            (try_lwt
                (* read notebook from file *)
                lwt name = 
                    try return (Kernel.M.filename_of_notebook_guid guid) 
                    with _ -> fail (Failure "bad_file") 
                in 
                lwt notebook = Lwt_io.(with_file ~mode:input (name ^ ".ipynb") read) in
                Server.respond_string ~status:`OK ~body:notebook ()
            with _ -> 
                not_found ())
            
        | `Notebooks_guid(guid) when meth = `PUT -> 
            (* save notebook *)
            (match body with
            | None -> not_found ()
            | Some(x) -> 
                try_lwt
                    lwt body = Cohttp_lwt_body.string_of_body body in
                    save_notebook guid body           
                with _ ->
                    not_found ())

        | `Notebooks_checkpoint(_) -> 
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Notebooks_checkpoint_id(_) -> not_found ()

        | `Clusters ->
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Kernels when meth = `POST -> 
            (*kernel_response req*)
            (try_lwt
                lwt notebook_guid = query_param "notebook" in 
                let kernel = Kernel.get_kernel
                    ~zmq ~path:notebook_path ~notebook_guid ~ip_addr:ws_addr
                    ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
                    ~zmq_heartbeat_port ~zmq_stdin_port 
                in
                Server.respond_string ~status:`OK
                    ~body:(kernel_id_json ~kernel_guid:kernel.Kernel.guid ~ws_addr ~ws_port) ()
            with _ ->
                not_found ())

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

let browser_command http_addr http_port =
   ("", [| "xdg-open"; "http://" ^ http_addr ^ ":" ^ string_of_int http_port |]) 

let run_servers () = 
    let http_server = http_server http_addr http_port in
    let _ = 
        Websocket.establish_server 
            (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string ws_addr, ws_port))
            Bridge.ws_init
    in
    let _ = Lwt_process.open_process_none (browser_command http_addr http_port) in
    http_server

let close_kernels () = 
    (* kill all running kernels *)
    Kernel.M.iter_kernels
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


