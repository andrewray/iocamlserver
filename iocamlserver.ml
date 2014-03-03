(* A http server for iocaml.  
   This will be merged into iocaml proper if it starts to work *)

open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix

(* some stuff we will need to set up dynamically (and figure out what they are!) *)
let kernel_id = "d827cffb-c1aa-4752-8a53-5b88aad189bd" (* guid for the kernel *)
let title = "Untitled0" (* notebook title *)
let data_project = "/home/andyman/dev/github/iocamlserver" (* not sure...notebook directory? *)
let data_notebook_id = "d8f01c67-33f6-48a8-8d4d-61a9d952e2bb" (* guid for the notebook instance *)

let generate_notebook () = 
    
    let static_url x = "/static/" ^ x in
    let mathjax_url = "http://cdn.mathjax.org/mathjax/latest/MathJax.js" in
    let base_project_url = "/" in
    let data_base_project_url = "/" in
    let data_base_kernel_url = "/" in
    let body_class = "notebook_app" in

    let style = Pages.notebook_stylesheet mathjax_url static_url in
    let header = Pages.notebook_header in
    let site = Pages.notebook_site in
    let script = Pages.notebook_scripts static_url in
    let page = Pages.page 
        title base_project_url static_url
        data_project data_base_project_url data_base_kernel_url
        data_notebook_id body_class
        style header site script
    in
    "<!DOCTYPE HTML>\n" ^ Cow.Html.to_string page

(* the json that's served for an empty notebook *)
let empty_notebook = "
{
    \"metadata\": { \"language\": \"ocaml\", \"name\": \"Untitled0\" },
    \"nbformat\": 3, \"nbformat_minor\": 0,
    \"worksheets\": [ { \"cells\": [], \"metadata\": {} } ]
}"

let kernel_id_message = "
{
    \"kernel_id\":\"" ^ kernel_id ^ "\",
    \"ws_url\":\"ws://127.0.0.1:8890\"
}
"

let header typ = 
    let h = Header.init () in
    let h = Header.add h "Content-Type" typ in
    let h = Header.add h "Server" "iocaml" in
    h

(* XXX caching headers *)
let header_none = Header.init ()
let header_html = header "text/html; charset=UTF-8"
let header_css = header "text/css"
let header_javascript = header "application/javascript"
let header_json = header "application/json"
let header_font = header "application/x-font-woff"

let header_of_extension filename = 
    if Filename.check_suffix filename ".js" then header_javascript
    else if Filename.check_suffix filename ".css" then header_css
    else if Filename.check_suffix filename ".ipynb" then header_json
    else if Filename.check_suffix filename ".woff" then header_font
    else header_none

(* kernel messages

 ipython/html/services/kernels/handlers.py
 ipython/html/static/notebook/js/notebook.js 

 /kernels?notebook=<guid> - send kernel id + ws_url, start kernel
 /kernels/<guid> - delete, stop kernel, status 204
 /kernels/<guid>/restart|interrupt - restart/interrupt kernel
 /kernels/<guid>/shell|iopub|stdin - websocket
*)

let kernel_response path =
    Server.respond_string ~status:`OK (*~headers:header_html*) ~body:kernel_id_message ()

(* notebook messages
 
 ipython/html/services/notebooks/handlers.py

 /notebooks
 /notebooks/<guid>
 /notebooks/<guid>/checkpoints
 /notebooks/<guid>/checkpoints/<id>

*)

(* root messages 

 ipython/html/notebook/handlers.py

 /<guid>
 /new - new notebook
 /<guid>/copy - copy and redirect
 /<name> - redirect to guid

*)

(* messages sent by paths in the url *)
module Path_messages = struct

    (* regex's for parsing url paths *)
    open Re

    let s = char '/'
    let d = char '-'
    let hex = alt [digit; (no_case (rg 'a' 'f'))]
    let hex n = repn hex n (Some(n))
    let guid = seq [ hex 8; d; hex 4; d; hex 4; d; hex 4; d; hex 12 ]
    let re_guid = compile guid

    let notebooks = str "/notebooks"
    let kernels = str "/kernels"
    let static = str "/static"

    let re_notebooks = compile notebooks
    let re_kernels = compile kernels
    let re_static = compile static

    type message =
        [ `Static
 
        | `Root
        | `Root_guid
        | `Root_new
        | `Root_copy
        | `Root_name
        
        | `Notebooks
        | `Notebooks_guid of string
        | `Notebooks_checkpoint of string
        | `Notebooks_checkpoint_id of string * string

        | `Kernels
        | `Kernels_guid of string
        | `Kernels_restart of string
        | `Kernels_interrupt of string
        
        | `Error_not_found ]

    type ws_message = 
        [ `Ws_shell of string
        | `Ws_iopub of string
        | `Ws_stdin of string
        | `Error_not_found ]

    let rec execl s = function
        | [] -> return `Error_not_found
        | (re,fn) :: tl ->
            return (try Some(fn (get_all (exec re s))) with _ -> None)
            >>= function
                | Some(x) -> return x
                | None -> execl s tl


    let compile_decode p (re,fn) = compile (seq (p::re)), fn

    let decode_notebooks = 
        let cp, guid = str "checkpoints", group guid in
        let re = [
            [eos], (fun _ -> `Notebooks);
            [s; guid; eos], (fun r -> `Notebooks_guid(r.(1)));
            [s; guid; s; cp; eos], (fun r -> `Notebooks_checkpoint(r.(1)));
            [s; guid; s; cp; s; guid; eos], (fun r -> `Notebooks_checkpoint_id(r.(1), r.(2)));
        ] in
        let re = List.map (compile_decode notebooks) re in
        fun path -> execl path re

    let decode_kernels = 
        let guid = group guid in
        let re = [
            [eos], (fun _ -> `Kernels);
            [s; guid; eos], (fun r -> `Kernels_guid(r.(1)));
            [s; guid; s; str "restart"; eos], (fun r -> `Kernels_restart(r.(1)));
            [s; guid; s; str "interrupt"; eos], (fun r -> `Kernels_interrupt(r.(1)));
        ] in
        let re = List.map (compile_decode kernels) re in
        fun path -> execl path re

    let decode_ws = 
        let guid = group guid in
        let re = [
            [s; guid; s; str "shell"; eos], (fun r -> `Ws_shell(r.(1)));
            [s; guid; s; str "iopub"; eos], (fun r -> `Ws_iopub(r.(1)));
            [s; guid; s; str "stdin"; eos], (fun r -> `Ws_stdin(r.(1)));
        ] in
        let re = List.map (compile_decode kernels) re in
        fun path -> execl path re

    let decode path = 
        let d = [
            (fun p -> p="" || p="/"), (fun _ -> return `Root);
            execp re_static, (fun _ -> return `Static);
            execp re_notebooks, decode_notebooks;
            execp re_kernels, decode_kernels;
        ] in
        let rec check = function
            | [] -> return `Error_not_found
            | (m,v)::tl ->
                return (m path) 
                >>= fun m -> if m then v path else check tl
        in
        check d

end

let make_server () =
    let callback conn_id ?body req =
        let open Path_messages in
        let uri = Request.uri req in
        let path = Uri.path uri in
        
        let not_found () = 
            Printf.eprintf "%s: ERROR: %s -> %s\n%!" 
                (Connection.to_string conn_id) (Uri.to_string uri) path;
            Server.respond_not_found ()
        in

        lwt decode = decode path in
        let ()  = 
            (* XXX log all messages that are not just serving notebook files *)
            if decode <> `Static then 
                Printf.eprintf "%s: %s -> %s\n%!" 
                    (Connection.to_string conn_id) (Uri.to_string uri) path;
        in

        match decode with

        | `Root ->
            let notebook = generate_notebook () in
            Server.respond_string ~status:`OK ~headers:header_html ~body:notebook ()

        | `Static -> 
            let fname = Server.resolve_file ~docroot:"ipython/html" ~uri:(Request.uri req) in
            if Sys.file_exists fname then 
                Server.respond_file ~headers:(header_of_extension fname) ~fname ()
            else not_found ()

        | `Root_guid -> not_found ()
        | `Root_new -> not_found ()
        | `Root_copy -> not_found ()
        | `Root_name -> not_found ()
        | `Notebooks -> not_found ()
        | `Notebooks_guid(_) -> 
            Server.respond_string ~status:`OK ~body:empty_notebook ()

        | `Notebooks_checkpoint(_) -> 
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Notebooks_checkpoint_id(_) -> not_found ()
        | `Kernels -> 
            kernel_response path

        | `Kernels_guid(_) -> not_found ()
        | `Kernels_restart(_) -> not_found ()
        | `Kernels_interrupt(_) -> not_found ()
        | `Error_not_found -> not_found ()
    in
    let conn_closed conn_id () =
        Printf.eprintf "%s: closed\n%!" (Connection.to_string conn_id)
    in
    let config = { Server.callback; conn_closed } in
    Server.create ~address:"0.0.0.0" ~port:8889 config

let rec ws_shell uri (stream,push) = 
    Lwt_stream.next stream >>= fun frame ->
    Lwt_io.eprintf "shell: %s\n" (Websocket.Frame.content frame) >>= fun () ->
    ws_shell uri (stream,push)

let ws_stdin uri (stream,push) = 
    Lwt_stream.next stream >>= fun frame ->
    Lwt_io.eprintf "stdin: %s\n" (Websocket.Frame.content frame) >>= fun () ->
    ws_shell uri (stream,push)

let ws_iopub uri (stream,push) = 
    Lwt_stream.next stream >>= fun frame ->
    Lwt_io.eprintf "iopub: %s\n" (Websocket.Frame.content frame) >>= fun () ->
    ws_shell uri (stream,push)

let ws_init uri (stream,push) = 
    Lwt_stream.next stream >>= fun frame ->
        (* display bring up cookie *)
        Lwt_io.eprintf "cookie: %s\n" (Websocket.Frame.content frame) >>= fun () ->
        (* handle each stream *)
        match_lwt Path_messages.decode_ws (Uri.path uri) with
        | `Ws_shell(guid) -> ws_shell uri (stream,push)
        | `Ws_stdin(guid) -> ws_stdin uri (stream,push)
        | `Ws_iopub(guid) -> ws_iopub uri (stream,push)
        | `Error_not_found -> Lwt.fail (Failure "invalid websocket url")

let run_server () = 
    let http_server = make_server () in
    let ws_server = 
        return 
            (Websocket.establish_server 
                (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", 8890))
                ws_init)
    in
    let rec wait_forever () = Lwt_unix.sleep 1000.0 >>= wait_forever in
    let ws_server = ws_server >>= fun _ -> wait_forever () in
    Lwt.join [ http_server; ws_server ]

let _ = Lwt_unix.run (run_server ())


