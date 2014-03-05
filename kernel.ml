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

module KMap = Map.Make(String)

(* notebook name to notebook-guid *)
let g_names : string KMap.t ref = ref KMap.empty

(* notebook=guid to kernel=guid *)
let g_notebooks : string KMap.t ref = ref KMap.empty

(* kernel-guid to kernel *)
let g_kernels : kernel KMap.t ref = ref KMap.empty

(* get notebook guid from name.  if it doesnt exist, add it *)
let notebook_of_name name = 
    try KMap.find name !g_names
    with Not_found ->
        let guid = Uuidm.(to_string (create `V4)) in
        g_names := KMap.add name guid !g_names;
        guid

(* get kernel for notebook *)
let kernel_of_notebook guid = 
    try Some (KMap.find guid !g_notebooks)
    with _ -> None

let add_notebook_to_kernel nguid kguid = 
    g_notebooks := KMap.add nguid kguid !g_notebooks

let get_kernel kernel_guid = KMap.find kernel_guid !g_kernels

let iter_kernels f = KMap.iter f !g_kernels

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

let write_connection_file 
    ~path
    ~kernel_guid ~ip_addr
    ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
    ~zmq_heartbeat_port ~zmq_stdin_port =

    let fname = Filename.concat path (kernel_guid ^ ".json") in
    let f = open_out fname in
    output_string f 
        (connection_file ~ip_addr ~zmq_shell_port ~zmq_iopub_port
            ~zmq_control_port ~zmq_heartbeat_port ~zmq_stdin_port);
    close_out f;
    fname

let init_kernel 
    ~zmq ~path ~ip_addr
    ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
    ~zmq_heartbeat_port ~zmq_stdin_port 
    =
    let kernel_guid = Uuidm.(to_string (create `V4)) in
    (* should be started with command line options *)
    let conn_file_name = write_connection_file
        ~path ~kernel_guid ~ip_addr
        ~zmq_shell_port ~zmq_iopub_port ~zmq_control_port
        ~zmq_heartbeat_port ~zmq_stdin_port 
    in
    let command = ("", [| "iocaml.top"; 
                            "-connection-file"; conn_file_name;
                            "-log"; "iocaml.log";
                      |]) 
    in
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
    {
        process = Lwt_process.open_process_none command;
        guid = kernel_guid;
        stdin = make_socket ZMQ.Socket.dealer ip_addr zmq_stdin_port;
        control = make_socket ZMQ.Socket.dealer ip_addr zmq_control_port;
        shell = shell_socket ip_addr zmq_shell_port;
        iopub = iopub_socket ip_addr zmq_iopub_port;
        heartbeat = heartbeat_socket ip_addr zmq_heartbeat_port;
    }



