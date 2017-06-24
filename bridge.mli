(*
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: bridge websockets and zmq sockets
 *
 *)

open Iocaml_zmq

type ws_stream = Websocket_lwt.Frame.t Lwt_stream.t
type ws_push = Websocket_lwt.Frame.t -> unit Lwt.t
type ws_comm = ws_stream * ws_push

val zmq_of_ws_message : string -> string list

val ws_of_zmq_message : string list -> string

val ws_to_zmq : int -> string -> ws_stream -> 'a Lwt_zmq.Socket.t -> unit Lwt.t

val zmq_to_ws : int -> string -> 'a Lwt_zmq.Socket.t -> ws_push -> unit Lwt.t

val ws_zmq_comms : int -> string -> 'a Lwt_zmq.Socket.t -> Uri.t -> ws_comm -> unit Lwt.t

val ws_init : int -> (Websocket_lwt.Connected_client.t -> unit Lwt.t)
