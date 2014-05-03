(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: kernel creation and guid mapping
 *
 *)

type kernel_args = 
    {
        log_file : string ref;
        init_file : string ref;
        completion : bool ref;
        object_info : bool ref;
    }

val kernel_args : kernel_args

type kernel =
    {
        process : Lwt_process.process_none;
        guid : string;
        (* zmq sockets *)
        stdin : unit -> [`Dealer] Lwt_zmq.Socket.t;
        control : unit -> [`Dealer] Lwt_zmq.Socket.t;
        shell : unit -> [`Dealer] Lwt_zmq.Socket.t;
        iopub : unit -> [`Sub] Lwt_zmq.Socket.t;
        heartbeat : unit -> [`Req] Lwt_zmq.Socket.t;
    }

(*module KMap : Map.S with type key = string*)

module M : sig

    val notebook_guid_of_filename : string -> string
    val notebook_guid_of_kernel_guid : string -> string

    val kernel_guid_of_notebook_guid : string -> string
    val kernel_guid_of_kernel : kernel -> string

    val kernel_of_notebook_guid : string -> kernel option
    val kernel_of_kernel_guid : string -> kernel option

    val filename_of_notebook_guid : string -> string
    val change_filename : string -> string -> string -> unit
        
    val add_kernel : string -> kernel -> unit
    val delete_kernel : string -> unit
    val iter_kernels : (string -> kernel -> unit) -> unit

    val dump_state : int -> unit

end

(** [resolve_addr host port] will resolve [host] and [port] into
    a Unix address info structure suitable for use with sockets. *)
val resolve_addr : string -> int -> Unix.sockaddr

val port_available : string -> int -> bool Lwt.t
val n_ports_available : string -> int -> int -> bool Lwt.t

val write_connection_file :
    path:string -> kernel_guid:string -> ip_addr:string ->
    zmq_shell_port:int -> zmq_iopub_port:int -> zmq_control_port:int ->
    zmq_heartbeat_port:int -> zmq_stdin_port:int -> string

val start_kernel :
    zmq:ZMQ.Context.t -> path:string -> notebook_guid:string -> ip_addr:string ->
    kernel Lwt.t

val get_kernel :
    zmq:ZMQ.Context.t -> path:string -> notebook_guid:string -> ip_addr:string -> 
    kernel Lwt.t

val close_kernel : string -> unit

