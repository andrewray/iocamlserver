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

(*module KMap : Map.S with type key = string*)

module M : sig

    val notebook_guid_of_filename : string -> string
    val notebook_guid_of_kernel_guid : string -> string
    val notebook_guid_of_kernel : kernel -> string

    val kernel_guid_of_filename : string -> string
    val kernel_guid_of_notebook_guid : string -> string
    val kernel_guid_of_kernel : kernel -> string

    val kernel_of_filename : string -> kernel option
    val kernel_of_notebook_guid : string -> kernel option
    val kernel_of_kernel_guid : string -> kernel option

    val filename_of_notebook_guid : string -> string
    val filename_of_kernel_guid : string -> string
    val filename_of_kernel : kernel -> string

    val add_kernel : string -> kernel -> unit
    val delete_kernel : string -> unit
    val iter_kernels : (string -> kernel -> unit) -> unit

end

val write_connection_file :
    path:string -> kernel_guid:string -> ip_addr:string ->
    zmq_shell_port:int -> zmq_iopub_port:int -> zmq_control_port:int ->
    zmq_heartbeat_port:int -> zmq_stdin_port:int -> string

val init_kernel :
    zmq:ZMQ.context -> path:string -> notebook_guid:string -> ip_addr:string ->
    zmq_shell_port:int -> zmq_iopub_port:int -> zmq_control_port:int ->
    zmq_heartbeat_port:int -> zmq_stdin_port:int -> kernel

