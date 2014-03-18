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

let address = ref "127.0.0.1"
let verbose = ref 0
let file_or_path = ref ""

let static_file_path = ref ""
let serve_uri_path = ref []
let serve_file_path = ref []

let iocamljs_kernel = ref ""

let log_file = ref ""
let init_file = ref ""

let () = 
    Arg.(parse (align [
        "-ip", Set_string(address), "<ip-address> ip address of server";
        "-js", Set_string(iocamljs_kernel), "<kernel> use iocamljs kernel";
        "-static", Set_string(static_file_path), "<dir> serve static files from dir";
        "-serve", Tuple([ String(fun s -> serve_uri_path := s :: !serve_uri_path); 
                          String(fun s -> serve_file_path := s :: !serve_file_path) ]), "<uri><path> serve files from uri";
        "-log", Set_string(log_file), "<file> kernel log file";
        "-init", Set_string(init_file), "<file> kernel init file";
        "-v", Unit(fun () -> incr verbose), " increase verbosity";
    ])
    (fun s -> file_or_path := s)
    "iocaml [options] [file-or-path]")

let notebook_path, file_to_open = Files.file_or_path !file_or_path 

let filename name = Filename.(concat notebook_path name)

let serve_files = List.map2 (fun a b -> a,b) !serve_uri_path !serve_file_path

(* process -js option and set up static file path *)
let () = 
    if !iocamljs_kernel <> "" then begin
        let share =
            try
                let ic = Unix.open_process_in ("opam config var share 2>/dev/null") in
                let r = input_line ic in
                let r =
                    let len = String.length r in
                    if len>0 && r.[len - 1] = '\r' then String.sub r 0 (len-1) else r
                in
                match Unix.close_process_in ic with
                | Unix.WEXITED 0 -> r
                | _ -> failwith ""
            with
            | _ -> failwith ("could not query opam for share directory")
        in
        static_file_path := share ^ "/iocamljs-kernel/profile"
    end

let () = 
    if !verbose > 0 then begin
        Printf.printf "ip address: '%s'\n" !address;
        Printf.printf "notebook path: '%s'\n" notebook_path;
        Printf.printf "file to open: '%s'\n" file_to_open;
        Printf.printf "extra static dir: '%s'\n" !static_file_path;
        List.iter (fun (u,p) ->
            Printf.printf "serve uri: '%s' -> '%s'\n" u p) serve_files;
        flush stdout;
    end

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
let header_binary = header "application/octet-stream"
let header_plain_user_charset = header "text/plain; charset=x-user-defined"

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

let kernel_id_json ~kernel_guid ~address ~ws_port = 
    let open Yojson.Basic in
    to_string ~std:true
        (`Assoc [
            "kernel_id", `String kernel_guid;
            "ws_url", `String ("ws://" ^ address ^ ":" ^ string_of_int ws_port);
        ])

let not_found () = 
    lwt () = if !verbose > 0 then Lwt_io.eprintf "Not_found\n" else return () in
    Server.respond_not_found ()
 
let notebook_list notebook_path = 
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
    Server.respond_string ~status:`OK ~body:(to_string ~std:true json) ()

(* read notebook from file *)
let send_notebook guid = 
    (try_lwt
        lwt name = 
            try return (Kernel.M.filename_of_notebook_guid guid) 
            with _ -> fail (Failure "bad_file") 
        in 
        lwt notebook = 
            Lwt_io.(with_file ~mode:input (filename (name ^ ".ipynb")) read) 
        in
        Kernel.M.dump_state !verbose;
        Server.respond_string ~status:`OK ~body:notebook ()
    with _ -> 
        not_found ())

let register_notebooks notebook_path = 
    lwt l = Files.list_notebooks notebook_path in
    Lwt_list.iter_s 
        (fun nb -> return (ignore (Kernel.M.notebook_guid_of_filename nb))) l

let serve_crunched_files uri = 
    (* serve from crunched file system  *)
    let fname = Server.resolve_file ~docroot:"" ~uri:uri in
    (match Filesys.read fname with
    | None -> not_found()
    | Some(x) -> 
        Server.respond_string ~status:`OK ~headers:(header_of_extension fname) ~body:x ())

let serve_static_files uri = 
    if !static_file_path <> "" then
        let fname = Server.resolve_file ~docroot:!static_file_path ~uri:uri in
        if Sys.file_exists fname then
            lwt () =
                if !verbose > 0 then Lwt_io.eprintf "  [  STATIC]: %s\n" fname 
                else return ()
            in
            Server.respond_file ~headers:(header_of_extension fname) ~fname:fname ()
        else
            serve_crunched_files uri
    else
        serve_crunched_files uri

(*
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
*)

let save_notebook guid body = 
    let old_filename = Kernel.M.filename_of_notebook_guid guid in
    (*lwt new_filename = get_filename_of_ipynb body in*)
    let new_filename, body = Files.prepare_ipynb_for_saving body in
    lwt () = Lwt_io.(with_file ~mode:output 
        (filename (new_filename ^ ".ipynb"))
        (fun f -> write f body))
    in
    let () = 
        if new_filename <> old_filename then
            Kernel.M.change_filename old_filename new_filename guid
    in
    Kernel.M.dump_state !verbose;
    Server.respond_string ~status:`OK ~headers:(header_date header_html) ~body:"" ()


let http_server address port ws_port notebook_path =
    let decode = Uri_paths.decode serve_files in
    
    let callback conn_id req body =
        let uri = Request.uri req in
        let meth = Request.meth req in
        let path = Uri.path uri in

        lwt decode = decode path in
        lwt ()  = 
            (* XXX log all messages that are not just serving notebook files *)
            if (!verbose > 0 && decode <> `Static) || (!verbose > 1) then 
                Lwt_io.eprintf "%s [%8s]: [%s] %s -> %s\n%!" 
                    (Connection.to_string conn_id)
                    (Code.string_of_method meth)
                    (Uri_paths.string_of_message decode) 
                    (Uri.to_string uri) path
            else
                return ()
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
            serve_static_files uri

        | `File(fname) ->
            lwt () = 
                if !verbose > 0 then Lwt_io.eprintf "  [    DATA] %s\n" fname
                else return ()
            in
            Server.respond_file ~headers:header_plain_user_charset ~fname:fname ()

        | `Root_guid(guid) -> 
            let notebook = Pages.generate_notebook_html 
                ~title:"IOCaml-Notebook" ~path:notebook_path 
                ~notebook_guid:guid ~kernel:!iocamljs_kernel
            in
            Kernel.M.dump_state !verbose;
            Server.respond_string ~status:`OK ~headers:header_html ~body:notebook ()

        | `Root_new -> 
            (* create new .ipynb file *)
            lwt name = Files.(list_notebooks notebook_path >>= new_notebook_name) in
            lwt () = Lwt_io.(with_file ~mode:output 
                (filename (name ^ ".ipynb")) 
                (fun f -> write f (Files.empty_notebook name)))
            in
            let guid = Kernel.M.notebook_guid_of_filename name in
            (* 302 Found, redirect to `Root_guid *)
            Server.respond_string ~status:`Found ~headers:(header_redirect guid) ~body:"" ()

        | `Root_name(name) ->
            (try_lwt 
                Server.respond_string ~status:`Found 
                    ~headers:(header_redirect (Kernel.M.notebook_guid_of_filename name)) 
                    ~body:"" ()
            with _ ->
                not_found ())

        | `Root_copy(guid) -> not_found ()

        | `Notebooks -> notebook_list notebook_path

        | `Notebooks_guid(guid) when meth = `GET ->
            (try_lwt
                (* read notebook from file *)
                lwt name = 
                    try return (Kernel.M.filename_of_notebook_guid guid) 
                    with _ -> fail (Failure "bad_file") 
                in 
                lwt notebook = Files.load_ipynb_for_serving filename name in
                Kernel.M.dump_state !verbose;
                Server.respond_string ~status:`OK ~body:notebook ()
            with _ -> 
                not_found ())
            
        | `Notebooks_guid(guid) when meth = `PUT -> 
            (* save notebook *)
            (try_lwt
                lwt body = Cohttp_lwt_body.to_string body in
                Kernel.M.dump_state !verbose;
                save_notebook guid body           
            with _ -> 
                not_found ())

        | `Notebooks_checkpoint(_) -> 
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Notebooks_checkpoint_id(_) -> not_found ()

        | `Clusters ->
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Kernels when meth = `POST -> 
            (try_lwt
                lwt notebook_guid = query_param "notebook" in 
                lwt kernel = Kernel.get_kernel
                    ~zmq ~path:notebook_path ~notebook_guid ~ip_addr:address
                    ~log_file:!log_file ~init_file:!init_file
                in
                Kernel.M.dump_state !verbose;
                Server.respond_string ~status:`OK
                    ~body:(kernel_id_json ~kernel_guid:kernel.Kernel.guid ~address ~ws_port) ()
            with _ ->
                not_found ())

        | `Kernels_guid(guid) when meth = `DELETE -> 
            let () = Kernel.close_kernel guid in
            Kernel.M.dump_state !verbose;
            Server.respond_string ~status:`OK ~body:"" ()

        | `Kernels_restart(guid) ->
            (try_lwt
                (* stop kernel *) 
                let () = Kernel.close_kernel guid in
                (* re-start kernel *)
                let notebook_guid = Kernel.M.notebook_guid_of_kernel_guid guid in
                lwt kernel = Kernel.get_kernel
                    ~zmq ~path:notebook_path ~notebook_guid ~ip_addr:address
                    ~log_file:!log_file ~init_file:!init_file
                in
                Kernel.M.dump_state !verbose;
                Server.respond_string ~status:`OK
                    ~body:(kernel_id_json ~kernel_guid:kernel.Kernel.guid ~address ~ws_port) ()
            with _ ->
                not_found ())

        | `Kernels_interrupt(guid) -> 
            (match Kernel.M.kernel_of_kernel_guid guid with
            | Some(kernel) ->
                kernel.Kernel.process#kill Sys.sigint; (* interrupt *)
                Kernel.M.dump_state !verbose;
                Server.respond_string ~status:`OK ~body:"" ()
            | None -> not_found ())

        | `Error_not_found | _ -> not_found ()
    in
    let conn_closed conn_id () = () in
    let config = { Server.callback; conn_closed } in
    Server.create ~address ~port config

let run_servers address notebook_path = 
    lwt () = register_notebooks notebook_path in

    (* find ports for http and websocket servers *)
    let rec find_port_pair port = 
        lwt ok = Kernel.n_ports_available address port 2 in
        if ok then return port
        else find_port_pair (port+2)
    in
    lwt http_port = find_port_pair 8888 in
    let ws_port = http_port + 1 in

    (* http server *)
    let http_server = http_server address http_port ws_port notebook_path in
    
    (* websocket server *)
    let _ = 
        Websocket.establish_server 
            (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string address, ws_port))
            (Bridge.ws_init !verbose)
    in

    (* start webbrowser, what about mac-osx? 'open'? *)
    let browser_command =
        if file_to_open <> "" then
            let guid = 
                Kernel.M.notebook_guid_of_filename 
                    (Filename.(chop_suffix file_to_open ".ipynb"))
            in
            ("", [| "xdg-open"; "http://" ^ address ^ ":" ^ 
                        string_of_int http_port ^ "/" ^ guid |]) 
        else
            ("", [| "xdg-open"; "http://" ^ address ^ ":" ^ string_of_int http_port |]) 
    in
    let _ = Lwt_process.open_process_none browser_command in

    http_server

let close_kernels () = 
    (* kill all running kernels *)
    Kernel.M.iter_kernels
        (fun _ v -> v.Kernel.process#terminate) 

let _ = 
    Sys.catch_break true;
    try 
        (*at_exit close_kernels;*)
        Lwt_unix.run (run_servers !address notebook_path)
    with
    | Sys.Break -> begin
        close_kernels ();
        (*ZMQ.term zmq*)
    end


