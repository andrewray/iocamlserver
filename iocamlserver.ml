(* A http server for iocaml.  
   This will be merged into iocaml proper if it starts to work *)

open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix

let kernel_id = "d827cffb-c1aa-4752-8a53-5b88aad189bd"
let title = "Untitled0" 
let data_project = "/home/andyman/dev/github/iocamlhttp" 
let data_notebook_id = "d8f01c67-33f6-48a8-8d4d-61a9d952e2bb" 

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
    \"metadata\": {
        \"language\": \"ocaml\",
        \"name\": \"Untitled0\"
    },
    \"nbformat\": 3,
    \"nbformat_minor\": 0,
    \"worksheets\": [
        {
            \"cells\": [],
            \"metadata\": {}
        }
    ]
}
"

let kernel_id_message = "
{
    kernel_id:\"" ^ kernel_id ^ "\";
    ws_url:\"\";
}
"

let is_prefix prefix of' = 
    let len = String.length prefix in
    (len <= String.length of') && 
    (String.sub of' 0 len = prefix)

(* we stop serving here, try and get the response as close to ipython as possible *)
let respond_not_found = 
    let h = Header.init () in
    let h = Header.add h "Content-Type" "text/html; charset=UTF-8" in
    let h = Header.add h "Server" "iocaml" in
    (* date *)
    let message = Cow.Html.to_string 
        <:html< <html><title>404: Not Found</title>
                <body>404: Not Found</body></html> >>
    in
    fun () -> Server.respond_string ~status:`Not_found ~headers:h ~body:message ()

let make_server () =
  let callback conn_id ?body req =
    let uri = Request.uri req in
    let path = Uri.path uri in
    Printf.eprintf "%s: %s -> %s\n%!" (Connection.to_string conn_id) (Uri.to_string uri) path;
    match path with
    |""|"/" -> (* notebook html generation *)
        let notebook = generate_notebook () in
        Server.respond_string ~status:`OK ~body:notebook ()
    | _ when is_prefix "/static" path -> (* notebook resources *)
        let fname = Server.resolve_file ~docroot:"ipython/html" ~uri:(Request.uri req) in
        if Sys.file_exists fname then Server.respond_file ~fname ()
        else respond_not_found ()

    | _  when is_prefix "/notebooks" path -> (* notebook json file *)
        Server.respond_string ~status:`OK ~body:empty_notebook ()
    
    | _ when is_prefix "/kernels" path -> 
        Server.respond_string ~status:`OK ~body:kernel_id_message ()

    | _ when is_prefix "/checkpoints" path -> 
        Server.respond_string ~status:`OK ~body:"[]" ()

    | _ -> respond_not_found ()

  in
  let conn_closed conn_id () =
    Printf.eprintf "%s: closed\n%!" (Connection.to_string conn_id)
  in
  let config = { Server.callback; conn_closed } in
  Server.create ~address:"0.0.0.0" ~port:8081 config

let _ = Lwt_unix.run (make_server ())

