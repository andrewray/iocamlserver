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
    \"ws_url\":\"\"
}
"

(*
let is_prefix prefix of' = 
    let len = String.length prefix in
    (len <= String.length of') && 
    (String.sub of' 0 len = prefix)

let split_by_prefix prefix str = 
    let plen,slen = String.(length prefix, length str) in
    String.sub str 0 plen, String.sub str plen (slen-plen)
*)

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

(* we stop serving here, try and get the response as close to ipython as possible *)
(*
let respond_not_found = 
    let message = Cow.Html.to_string 
        <:html< <html><title>404: Not Found</title>
                <body>404: Not Found</body></html> >>
    in
    fun () -> Server.respond_string ~status:`Not_found ~headers:header_html ~body:message ()
*)

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

(*
let notebook_response path = 
    Server.respond_string ~status:`OK ~body:empty_notebook ()
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
        | Static
 
        | Root
        | Root_guid
        | Root_new
        | Root_copy
        | Root_name
        
        | Notebooks
        | Notebooks_guid of string
        | Notebooks_checkpoint of string
        | Notebooks_checkpoint_id of string * string

        | Kernels
        | Kernels_guid of string
        | Kernels_restart of string
        | Kernels_interrupt of string
        | Kernels_shell of string
        | Kernels_iopub of string
        | Kernels_stdin of string
        
        | Error_not_found

    let rec execl s = function
        | [] -> Error_not_found
        | (re, fn) :: tl ->
            try 
                fn (get_all (exec re s))
            with _ -> 
                execl s tl

    let compile_decode p (re,fn) = compile (seq (p::re)), fn

    let decode_notebooks = 
        let re = [
            [eos], 
                (fun _ -> Notebooks);
            [s; group guid; eos],
                (fun r -> Notebooks_guid(r.(1)));
            [s; group guid; s; str "checkpoints"; eos],
                (fun r -> Notebooks_checkpoint(r.(1)));
            [s; group guid; s; str "checkpoints"; s; group guid; eos],
                (fun r -> Notebooks_checkpoint_id(r.(1), r.(2)));
        ] in
        let re = List.map (compile_decode notebooks) re in
        fun path -> execl path re

    let decode_kernels = 
        let re = [
            [eos],
                (fun _ -> Kernels);
            [s; group guid; eos],
                (fun r -> Kernels_guid(r.(1)));
            [s; group guid; s; str "restart"; eos],
                (fun r -> Kernels_restart(r.(1)));
            [s; group guid; s; str "interrupt"; eos],
                (fun r -> Kernels_interrupt(r.(1)));
            [s; group guid; s; str "shell"; eos],
                (fun r -> Kernels_shell(r.(1)));
            [s; group guid; s; str "iopub"; eos],
                (fun r -> Kernels_iopub(r.(1)));
            [s; group guid; s; str "stdin"; eos],
                (fun r -> Kernels_stdin(r.(1)));
        ] in
        let re = List.map (compile_decode kernels) re in
        fun path -> execl path re

    let decode path = 
        if path = "" || path = "/" then Root
        else if execp re_static path then Static
        else if execp re_notebooks path then decode_notebooks path
        else if execp re_kernels path then decode_kernels path
        else Error_not_found

end

(*
let make_server () =
  let callback conn_id ?body req =
    
    let uri = Request.uri req in
    let path = Uri.path uri in

    match path with
    | "" | "/" -> (* notebook html generation *)
        let notebook = generate_notebook () in
        Printf.eprintf "%s: %s -> %s\n%!" (Connection.to_string conn_id) (Uri.to_string uri) path;
        Server.respond_string ~status:`OK ~headers:header_html ~body:notebook ()

    | _ when is_prefix "/static" path -> (* notebook resources *)
        let fname = Server.resolve_file ~docroot:"ipython/html" ~uri:(Request.uri req) in
        if Sys.file_exists fname then 
            Server.respond_file ~headers:(header_of_extension fname) ~fname ()
        else respond_not_found ()

    | _  when is_prefix "/notebooks" path -> (* notebook json file *)
        Printf.eprintf "%s: %s -> %s\n%!" (Connection.to_string conn_id) (Uri.to_string uri) path;
        notebook_response path
    
    | _ when is_prefix "/kernels" path -> 
        Printf.eprintf "%s: %s -> %s\n%!" (Connection.to_string conn_id) (Uri.to_string uri) path;
        kernel_response path

    | _ when is_prefix "/checkpoints" path -> 
        Printf.eprintf "%s: %s -> %s\n%!" (Connection.to_string conn_id) (Uri.to_string uri) path;
        Server.respond_string ~status:`OK ~body:"[]" ()

    | _ -> 
        Printf.eprintf "%s: %s -> %s\n%!" (Connection.to_string conn_id) (Uri.to_string uri) path;
        respond_not_found ()

  in
  let conn_closed conn_id () =
    Printf.eprintf "%s: closed\n%!" (Connection.to_string conn_id)
  in
  let config = { Server.callback; conn_closed } in
  Server.create ~address:"0.0.0.0" ~port:8081 config
*)


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

        let decode = decode path in
        let ()  = 
            (* XXX log all messages that are not just serving notebook files *)
            if decode <> Static then 
                Printf.eprintf "%s: %s -> %s\n%!" 
                    (Connection.to_string conn_id) (Uri.to_string uri) path;
        in

        match decode with

        | Root ->
            let notebook = generate_notebook () in
            Server.respond_string ~status:`OK ~headers:header_html ~body:notebook ()

        | Static -> 
            let fname = Server.resolve_file ~docroot:"ipython/html" ~uri:(Request.uri req) in
            if Sys.file_exists fname then 
                Server.respond_file ~headers:(header_of_extension fname) ~fname ()
            else not_found ()

        | Root_guid -> not_found ()
        | Root_new -> not_found ()
        | Root_copy -> not_found ()
        | Root_name -> not_found ()
        | Notebooks -> not_found ()
        | Notebooks_guid(_) -> 
            Server.respond_string ~status:`OK ~body:empty_notebook ()

        | Notebooks_checkpoint(_) -> 
                Server.respond_string ~status:`OK ~body:"[]" ()

        | Notebooks_checkpoint_id(_) -> not_found ()
        | Kernels -> 
            kernel_response path

        | Kernels_guid(_) -> not_found ()
        | Kernels_restart(_) -> not_found ()
        | Kernels_interrupt(_) -> not_found ()
        | Kernels_shell(_) -> not_found ()
        | Kernels_iopub(_) -> not_found ()
        | Kernels_stdin(_) -> not_found ()
        | Error_not_found -> not_found ()
    in
    let conn_closed conn_id () =
        Printf.eprintf "%s: closed\n%!" (Connection.to_string conn_id)
    in
    let config = { Server.callback; conn_closed } in
    Server.create ~address:"0.0.0.0" ~port:8081 config

let _ = Lwt_unix.run (make_server ())


