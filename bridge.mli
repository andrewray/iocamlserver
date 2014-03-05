(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: bridge websockets and zmq sockets
 *
 *)

type ws_stream = Websocket.Frame.t Lwt_stream.t
type ws_push = Websocket.Frame.t option -> unit
type ws_comm = ws_stream * ws_push 

val zmq_of_ws_message : string -> string list

val ws_of_zmq_message : string list -> string

val ws_to_zmq : string -> ws_stream -> 'a Lwt_zmq.Socket.t -> unit Lwt.t

val zmq_to_ws : string -> 'a Lwt_zmq.Socket.t -> ws_push -> unit Lwt.t

val ws_zmq_comms : string -> 'a Lwt_zmq.Socket.t -> Uri.t -> ws_comm -> unit Lwt.t

val ws_init : Uri.t -> ws_comm -> unit Lwt.t

