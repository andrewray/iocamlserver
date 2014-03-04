(* A http server for iocaml.  
   This will be merged into iocaml proper if it starts to work 

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

(* zmq initialization *)
let zmq = ZMQ.init ()

(* some stuff we will need to set up dynamically (and figure out what they are!) *)
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
let empty_notebook title = 
    let open Yojson.Basic in
    to_string 
        (`Assoc [
            "metadata", `Assoc [
                "language", `String "ocaml";
                "name", `String title;
            ];
            "nbformat", `Int 3;
            "nbformat_minor", `Int 0;
            "worksheets", `List [
                `Assoc [
                    "cells", `List [];
                    "metadata", `Assoc [];
                ];
            ];
        ])

let kernel_id_message kernel_guid ws_addr ws_port = 
    let open Yojson.Basic in
    to_string 
        (`Assoc [
            "kernel_id", `String kernel_guid;
            "ws_url", `String ("ws://" ^ ws_addr ^ ":" ^ string_of_int ws_port);
        ])

let connection_file  
    ~ip_addr
    ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
    ~zmq_heartbeat_port ~zmq_stdin_port
    = 
    let open Yojson.Basic in
    to_string 
        (`Assoc [
          "stdin_port", `Int zmq_stdin_port; 
          "ip", `String ip_addr; 
          "control_port", `Int zmq_control_port; 
          "hb_port", `Int zmq_heartbeat_port; 
          "signature_scheme", `String "hmac-sha256"; 
          "key", `String ""; 
          "shell_port", `Int zmq_shell_port; 
          "transport", `String "tcp"; 
          "iopub_port", `Int zmq_iopub_port
        ])

type kernel =
    {
        process : Lwt_process.process_none;
        guid : string;
        (* zmq sockets *)
        stdin : [`Dealer] Lwt_zmq.Socket.t;
        control : [`Dealer] Lwt_zmq.Socket.t;
        shell : [`Dealer] Lwt_zmq.Socket.t;
        iopub : [`Sub] Lwt_zmq.Socket.t;
        heartbeat : [`Req] Lwt_zmq.Socket.t;
    }

module Kernel_map = Map.Make(String)

let g_kernels : kernel Kernel_map.t ref = ref Kernel_map.empty
let g_notebooks : string Kernel_map.t ref = ref Kernel_map.empty

let kernel_of_notebooks notebook_guid = Kernel_map.find notebook_guid !g_notebooks
let kernel_data kernel_guid = Kernel_map.find kernel_guid !g_kernels

let write_connection_file 
    ~kernel_guid ~ip_addr
    ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
    ~zmq_heartbeat_port ~zmq_stdin_port =

    let cwd = Unix.getcwd () in
    let fname = Filename.concat cwd (kernel_guid ^ ".json") in
    let f = open_out fname in
    output_string f 
        (connection_file ~ip_addr ~zmq_shell_port ~zmq_iopub_port
            ~zmq_control_port ~zmq_heartbeat_port ~zmq_stdin_port);
    close_out f;
    fname

let kernel_response req =
    (* I think at this point we create the kernel and add a mapping *)
    let kernel_guid = Uuidm.(to_string (create `V4)) in
    let conn_file_name = write_connection_file
        ~kernel_guid ~ip_addr:ws_addr
        ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
        ~zmq_heartbeat_port ~zmq_stdin_port 
    in
    let command = ("", [| "iocaml.top"; 
                            "-connection-file"; conn_file_name;
                            "-log"; "iocaml.log";
                      |]) 
    in
    lwt notebook_guid =  
        let uri = Request.uri req in
        match Uri.get_query_param uri "notebook" with
        | None -> Lwt.fail (Failure "/kernels expecting notebook id")
        | Some(x) -> return x
    in
    lwt () = Lwt_io.eprintf "kernel_guid: %s\n" kernel_guid in
    lwt () = Lwt_io.eprintf "notebook_guid: %s\n" notebook_guid in
    let make_socket typ addr port = 
        let socket = ZMQ.Socket.(create zmq typ) in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in
    (* see kernel/channels.py *)
    let shell_socket addr port = 
        let socket = ZMQ.Socket.(create zmq dealer) in
        let () = ZMQ.Socket.set_identity socket "12345678123456781234567812345678" in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in
    let iopub_socket addr port = 
        let socket = ZMQ.Socket.(create zmq sub) in
        let () = ZMQ.Socket.subscribe socket "" in
        let () = ZMQ.Socket.set_identity socket "12345678123456781234567812345678" in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in
    let heartbeat_socket addr port = 
        let socket = ZMQ.Socket.(create zmq req) in
        let () = ZMQ.Socket.set_linger_period socket 0 in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in
    let k = 
        {
            process = Lwt_process.open_process_none command;
            guid = kernel_guid;
            stdin = make_socket ZMQ.Socket.dealer ws_addr zmq_stdin_port;
            control = make_socket ZMQ.Socket.dealer ws_addr zmq_control_port;
            shell = shell_socket ws_addr zmq_shell_port;
            iopub = iopub_socket ws_addr zmq_iopub_port;
            heartbeat = heartbeat_socket ws_addr zmq_heartbeat_port;
        }
    in
    let () = 
        g_notebooks := Kernel_map.add notebook_guid k.guid !g_notebooks;
        g_kernels := Kernel_map.add kernel_guid k !g_kernels
    in
    Server.respond_string ~status:`OK 
        ~body:(kernel_id_message kernel_guid ws_addr ws_port) ()

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
    let status = str "/status"

    let re_notebooks = compile notebooks
    let re_kernels = compile kernels
    let re_static = compile static
    let re_status = compile status

    type message =
        [ `Static
 
        | `Status

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
            execp re_status, (fun _ -> return `Status);
        ] in
        let rec check = function
            | [] -> return `Error_not_found
            | (m,v)::tl ->
                return (m path) 
                >>= fun m -> if m then v path else check tl
        in
        check d

end

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

let header_of_extension filename = 
    if Filename.check_suffix filename ".js" then header_javascript
    else if Filename.check_suffix filename ".css" then header_css
    else if Filename.check_suffix filename ".ipynb" then header_json
    else if Filename.check_suffix filename ".woff" then header_font
    else header_none

let make_server address port =
    let callback conn_id ?body req =
        let open Path_messages in
        let uri = Request.uri req in
        let meth = Request.meth req in
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
    
        | `Status ->
            let static_url x = "/static/" ^ x in
            let guids = List.map fst (Kernel_map.bindings !g_kernels) in
            Server.respond_string ~status:`OK ~headers:header_html 
                ~body:(Cow.Html.to_string 
                    (Pages.(page "status" "" static_url "" "" "" "" "no_class" 
                          empty empty (status_site guids) empty))) ()

        | `Root_guid -> not_found ()
        | `Root_new -> not_found ()
        | `Root_copy -> not_found ()
        | `Root_name -> not_found ()
        | `Notebooks -> not_found ()
        | `Notebooks_guid(_) when meth = `GET -> 
            Server.respond_string ~status:`OK ~body:(empty_notebook "Untitled0") ()

        | `Notebooks_guid(_) when meth = `PUT -> 
            (* save notebook *)
            not_found ()

        | `Notebooks_checkpoint(_) -> 
            Server.respond_string ~status:`OK ~body:"[]" ()

        | `Notebooks_checkpoint_id(_) -> not_found ()
        | `Kernels -> 
            kernel_response req

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

(*
{
    "header":
        {
            "msg_id":"1EF4E8EB10A04B3580C794F453AE74C3",
            "username":"username",
            "session":"D154E0C448ED4BD0870FF06B6D82AB22",
            "msg_type":"execute_request"
        },
    "metadata":{},
    "content":
        {
            "code":"let a = 1",
            "silent":false,
            "store_history":true,
            "user_variables":[],
            "user_expressions":{},
            "allow_stdin":true
        },
    "parent_header":{}
}
*)

let zmq_of_ws_message data = 
    let open Yojson.Basic in
    let data = from_string data in
    match data with
    | `Assoc l ->
        [   
            "<IDS|MSG>";
            "";
            to_string (List.assoc "header" l);
            to_string (List.assoc "parent_header" l);
            to_string (List.assoc "metadata" l);
            to_string (List.assoc "content" l);
        ]
    | _ -> raise (Failure "deserialize_ws")

(*
{
    "parent_header":
        {
            "username":"username",
            "session":"E61C23EFDFAA4D3E8376E2EBB92898BC",
            "msg_id":"EBDF3748CE8F4A84891C19C11BAA99A4",
            "msg_type":"execute_request"
        },
    "msg_type":"status",
    "msg_id":"506786e3-3bb8-4d75-aa43-956180b2867b",
    "content":{"execution_state":"idle"},
    "header":
        {
            "username":"username",
            "session":"E61C23EFDFAA4D3E8376E2EBB92898BC",
            "msg_id":"506786e3-3bb8-4d75-aa43-956180b2867b",
            "msg_type":"status"
        },
    "metadata":{}
}
*)

let ws_of_zmq_message data = 
    let open Yojson.Basic in
    let rec find = function
        | [] -> raise (Failure "bad zmq message")
        | "<IDS|MSG>"::_::h::p::m::c::_ -> from_string h, from_string p,
                                           from_string m, from_string c
        | h::t -> find t
    in
    let header,parent,meta,content = find data in
    let extract data = 
        match header with `Assoc l -> (List.assoc data l) | _ -> `String "error"
    in
    to_string
        (`Assoc [
            "parent_header", parent;
            "msg_type", extract "msg_type";
            "msg_id", extract "msg_id";
            "content", content;
            "header", header;
            "metadata", meta;
        ])

let ws_to_zmq name stream socket = 
    lwt frame = Lwt_stream.next stream in
    let data = Websocket.Frame.content frame in
    lwt () = Lwt_io.eprintf "%s: %s\n" name data in
    Lwt_zmq.Socket.send_all socket (zmq_of_ws_message data)

let zmq_to_ws name socket push = 
    lwt frames = Lwt_zmq.Socket.recv_all socket in
    lwt () = 
        Lwt_list.iter_s (Lwt_io.eprintf "%s: %s\n" name) frames 
    in
    let frame = ws_of_zmq_message frames in
    Lwt.wrap (fun () -> push (Some (Websocket.Frame.of_string frame))) 

let rec ws_zmq_comms name socket uri (stream,push) = 
    lwt () = Lwt_io.eprintf "ws_zmq_comms: %s\n" name in
    lwt _ = zmq_to_ws name socket push <?> ws_to_zmq name stream socket in
    ws_zmq_comms name socket uri (stream,push)

let ws_init uri (stream,push) = 
    Lwt_stream.next stream >>= fun frame ->
        (* we get one special message per channel, after which it's comms time *)
        lwt () = Lwt_io.eprintf "cookie: %s\n" (Websocket.Frame.content frame) in
        let find guid = Kernel_map.find guid !g_kernels in
        match_lwt Path_messages.decode_ws (Uri.path uri) with
        | `Ws_shell(guid) -> ws_zmq_comms "shell" (find guid).shell uri (stream,push)
        | `Ws_stdin(guid) -> ws_zmq_comms "stdin" (find guid).stdin uri (stream,push)
        | `Ws_iopub(guid) -> ws_zmq_comms "iopub" (find guid).iopub uri (stream,push)
        | `Error_not_found -> Lwt.fail (Failure "invalid websocket url")

let run_server () = 
    let http_server = make_server http_addr http_port in
    let ws_server = 
        return 
            (Websocket.establish_server 
                (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string ws_addr, ws_port))
                ws_init)
    in
    let rec wait_forever () = Lwt_unix.sleep 1000.0 >>= wait_forever in
    let ws_server = ws_server >>= fun _ -> wait_forever () in
    Lwt.join [ http_server; ws_server ]

let close_kernels () = 
    Kernel_map.iter 
        (fun _ v -> 
            eprintf "killing kernel: %s\n" v.guid;
            v.process#terminate) !g_kernels

let _ = 
    Sys.catch_break true;
    try 
        (*at_exit close_kernels;*)
        Lwt_unix.run (run_server ())
    with
    | Sys.Break -> begin
        close_kernels ();
        (*ZMQ.term zmq*)
    end


