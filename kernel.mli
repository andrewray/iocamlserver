(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: kernel creation and guid mapping
 *
 *)

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

module KMap : Map.S with type key = string

val notebook_of_name : string -> string

(** get the kernel guid from the notebook guid *)
val kernel_of_notebook : string -> string option

(** get the kernel data from the kernel guid *)
val get_kernel : string -> kernel

val iter_kernels : (string -> kernel -> unit) -> unit

val write_connection_file :
    path:string -> kernel_guid:string -> ip_addr:string ->
    zmq_shell_port:int -> zmq_iopub_port:int -> zmq_control_port:int ->
    zmq_heartbeat_port:int -> zmq_stdin_port:int -> string

val init_kernel :
    zmq:ZMQ.context -> 
    path:string -> ip_addr:string ->
    zmq_shell_port:int -> zmq_iopub_port:int -> zmq_control_port:int ->
    zmq_heartbeat_port:int -> zmq_stdin_port:int -> kernel


