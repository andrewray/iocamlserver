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
        stdin : unit -> [`Dealer] Lwt_zmq.Socket.t;
        control : unit -> [`Dealer] Lwt_zmq.Socket.t;
        shell : unit -> [`Dealer] Lwt_zmq.Socket.t;
        iopub : unit -> [`Sub] Lwt_zmq.Socket.t;
        heartbeat : unit -> [`Req] Lwt_zmq.Socket.t;
    }

module M = struct

    (* there are 4 data elements to map between,

        1. filename
        2. notebook_guid
        3. kernel_guid
        4. kernel 

        filenames are either got from File.list_notebook, which are initiated 
        from the dashboard, or by a new notebook request, copy etc.

        notebook_guids are derived from filenames.
        kernel_guids are derived from notebook_guids.

        kernels are created and put in a map keyed by kernel_guids when the kernel
        is created.  they may also be destoyed, or restarted.

        The forward mapping filename->notebook_guid->kernel_guid->kernel is
        straightforward.

        The inverse mapping uses a map from kernel_guids to filenames.

    *)

    module KMap = Map.Make(String)

    let (@@) f g x = g (f x)

    (* guid generation seed *)
    let seed = 
        match Uuidm.of_string "65506491-a9a4-439f-be2d-03be8732c88e" with
        | None -> failwith "couldn't init seed"
        | Some(x) -> x

    (* kernel_guid -> kernel map *)
    let kernels : kernel KMap.t ref = ref KMap.empty
    (* kernel_guid -> filename map *)
    let filenames : string KMap.t ref = ref KMap.empty

    (* forward accessor functions *)

    let rec notebook_guid_of_filename filename =
        (* everytime we ask for a notebook guid, add the reverse mapping *)
        let guid = Uuidm.(to_string (v3 seed filename)) in
        filenames := KMap.add (kernel_guid_of_notebook_guid guid) filename !filenames; 
        guid
    
    and kernel_guid_of_notebook_guid notebook_guid = 
        Uuidm.(to_string (v3 seed notebook_guid))
 
    let kernel_guid_of_filename = 
        notebook_guid_of_filename @@ kernel_guid_of_notebook_guid

    let kernel_of_kernel_guid kernel_guid = 
        try Some(KMap.find kernel_guid !kernels)
        with _ -> None
    
    let kernel_of_notebook_guid = kernel_guid_of_notebook_guid @@ kernel_of_kernel_guid

    let kernel_of_filename = notebook_guid_of_filename @@ kernel_of_notebook_guid

    (* reverse accessor functions *)

    let filename_of_kernel_guid kernel_guid = 
        (* dont expect this to fail, but it could *)
        try KMap.find kernel_guid !filenames
        with _ -> failwith "could not map kernel_guid to filename"

    let notebook_guid_of_kernel_guid = filename_of_kernel_guid @@ notebook_guid_of_filename
    let filename_of_notebook_guid = kernel_guid_of_notebook_guid @@ filename_of_kernel_guid

    let kernel_guid_of_kernel kernel = kernel.guid
    let notebook_guid_of_kernel = kernel_guid_of_kernel @@ notebook_guid_of_kernel_guid
    let filename_of_kernel = kernel_guid_of_kernel @@ filename_of_kernel_guid

    (* add/delete active kernels from the map *)

    let add_kernel kernel_guid kernel = kernels := KMap.add kernel_guid kernel !kernels
    let delete_kernel kernel_guid = kernels := KMap.remove kernel_guid !kernels
    let iter_kernels f = KMap.iter f !kernels

end

(* check if the given port is free 
 * XXX not sure about this *)
let port_available addr port = 
    let s = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    lwt status = 
        try_lwt
            Lwt_unix.(bind s (ADDR_INET(Unix.inet_addr_of_string addr, port)));
            Lwt.return true
        with _ ->
            Lwt.return false
    in
    lwt () = Lwt_unix.close s in
    Lwt.return status

let rec n_ports_available addr port n = 
    if n=0 then Lwt.return true
    else
        lwt available = port_available addr port in
        if available then n_ports_available addr (port+1) (n-1)
        else Lwt.return false

let rec find_zmq_port_range addr = 
    let port = Random.int 40000 + 20000 in (* between 20,000 + 60,000 *)
    lwt available = n_ports_available addr port 5 in
    if available then Lwt.return port
    else find_zmq_port_range addr

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

let start_kernel ~zmq ~path ~notebook_guid ~ip_addr =
    let kernel_guid = M.kernel_guid_of_notebook_guid notebook_guid in
    
    (* find free ports *)
    lwt p = find_zmq_port_range ip_addr in
    let zmq_shell_port = p+0 in
    let zmq_iopub_port = p+1 in
    let zmq_control_port= p+2 in
    let zmq_heartbeat_port = p+3 in
    let zmq_stdin_port = p+4 in

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
    let make_socket typ addr port () = 
        let socket = ZMQ.Socket.(create zmq typ) in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in

    let identity () = Uuidm.(to_string (create `V4)) in

    (* see kernel/channels.py *)
    let shell_socket addr port () = 
        let socket = ZMQ.Socket.(create zmq dealer) in
        let () = ZMQ.Socket.set_identity socket (identity()) in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in
    let iopub_socket addr port () = 
        let socket = ZMQ.Socket.(create zmq sub) in
        let () = ZMQ.Socket.subscribe socket "" in
        let () = ZMQ.Socket.set_identity socket (identity()) in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in
    let heartbeat_socket addr port () = 
        let socket = ZMQ.Socket.(create zmq req) in
        let () = ZMQ.Socket.set_linger_period socket 0 in
        let () = ZMQ.Socket.connect socket ("tcp://" ^ addr ^ ":" ^ string_of_int port) in
        Lwt_zmq.Socket.of_socket socket
    in
    let kernel = 
        {
            process = Lwt_process.open_process_none command;
            guid = kernel_guid;
            stdin = make_socket ZMQ.Socket.dealer ip_addr zmq_stdin_port;
            control = make_socket ZMQ.Socket.dealer ip_addr zmq_control_port;
            shell = shell_socket ip_addr zmq_shell_port;
            iopub = iopub_socket ip_addr zmq_iopub_port;
            heartbeat = heartbeat_socket ip_addr zmq_heartbeat_port;
        }
    in
    (* add kernel *)
    M.add_kernel kernel_guid kernel;
    Lwt.return kernel
    
let get_kernel ~zmq ~path ~notebook_guid ~ip_addr =
    match M.kernel_of_notebook_guid notebook_guid with
    | Some(k) -> Lwt.return k
    | None -> 
        start_kernel ~zmq ~path ~notebook_guid ~ip_addr


