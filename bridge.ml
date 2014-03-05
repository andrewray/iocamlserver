(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: bridge websockets and zmq sockets
 *
 *)
open Lwt

type ws_stream = Websocket.Frame.t Lwt_stream.t
type ws_push = Websocket.Frame.t option -> unit
type ws_comm = ws_stream * ws_push 

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
        (* parse the uri to find out which socket we want *)
        match_lwt Uri_paths.decode_ws (Uri.path uri) with
        | `Ws_shell(guid) -> ws_zmq_comms "shell" Kernel.((get_kernel guid).shell) uri (stream,push)
        | `Ws_stdin(guid) -> ws_zmq_comms "stdin" Kernel.((get_kernel guid).stdin) uri (stream,push)
        | `Ws_iopub(guid) -> ws_zmq_comms "iopub" Kernel.((get_kernel guid).iopub) uri (stream,push)
        | `Error_not_found -> Lwt.fail (Failure "invalid websocket url")


