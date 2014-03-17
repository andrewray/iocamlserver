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

    (* map between 

        1. filename
        2. notebook_guid
        3. kernel_guid
        4. kernel 

       There are some issues around renaming notebooks and tracking
       which filename, notebook guid and kernel.
    *)

    module M = Map.Make(String)

    (* maintain map of kernel_guids to kernels *)
    let kernels : kernel M.t ref = ref M.empty

    let kernel_of_kernel_guid kernel_guid = 
        try Some(M.find kernel_guid !kernels)
        with _ -> None

    let add_kernel kernel_guid kernel = kernels := M.add kernel_guid kernel !kernels
    let delete_kernel kernel_guid = kernels := M.remove kernel_guid !kernels
    let iter_kernels f = M.iter f !kernels

    (*  *)
    let seed = 
        match Uuidm.of_string "65506491-a9a4-439f-be2d-03be8732c88e" with
        | None -> failwith "couldn't init seed"
        | Some(x) -> x

    type str_map = 
        {
            find : string -> string;
            add : string -> string -> unit;
            remove : string -> unit;
            iter : (string -> string -> unit) -> unit
        }

    let make_str_map () = 
        let map : string M.t ref = ref M.empty in
        let find k = M.find k !map in
        let add k d = map := M.add k d !map in
        let remove k = map := M.remove k !map in
        let iter f = M.iter f !map in
        { find; add; remove; iter }

    type str_map_r = { f : str_map; b : str_map; }

    let make_str_map_r () = 
        let f, b = make_str_map (), make_str_map () in
        let mk f b = 
            {
                find = f.find;
                add = (fun k d -> f.add k d; b.add d k);
                remove = (fun k -> b.remove (f.find k); f.remove k);
                iter = f.iter;
            }
        in
        { f = mk f b; b = mk b f; }

    let f_ng = make_str_map_r () (* filename <-> notebook_guid *)

    let notebook_guid_of_filename filename = 
        try f_ng.f.find filename
        with _ ->
            f_ng.f.add filename Uuidm.(to_string (create `V4));
            f_ng.f.find filename

    let filename_of_notebook_guid notebook_guid = f_ng.b.find notebook_guid

    let change_filename old_filename new_filename notebook_guid = 
        f_ng.f.remove old_filename;
        f_ng.f.add new_filename notebook_guid

    let ng_kg = make_str_map_r ()

    let kernel_guid_of_notebook_guid notebook_guid = 
        try ng_kg.f.find notebook_guid
        with _ ->
            ng_kg.f.add notebook_guid Uuidm.(to_string (v3 seed notebook_guid));
            ng_kg.f.find notebook_guid

    let notebook_guid_of_kernel_guid kernel_guid = ng_kg.b.find kernel_guid

    let kernel_guid_of_kernel kernel = kernel.guid

    let kernel_of_notebook_guid notebook_guid = 
        kernel_of_kernel_guid (kernel_guid_of_notebook_guid notebook_guid)

    let dump_state verbose = 
        if verbose > 1 then begin
            Printf.printf "%36s -> %36s -> %36s\n" "filename" "notebook_guid" "filename";
            f_ng.f.iter (fun k v -> Printf.printf "%36s -> %36s -> %36s\n" k v (f_ng.b.find v));
            Printf.printf "%36s -> %36s -> %36s\n" "notebook_guid" "kernel_guid" "notebook_guid";
            ng_kg.f.iter (fun k v -> Printf.printf "%36s -> %36s -> %36s\n" k v (ng_kg.b.find v));
            flush stdout
        end

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

let start_kernel 
    ~zmq ~path ~notebook_guid ~ip_addr 
    ~log_file ~init_file =
    let kernel_guid = M.kernel_guid_of_notebook_guid notebook_guid in
    
    (* find free ports *)
    lwt p = find_zmq_port_range ip_addr in
    let zmq_shell_port = p+0 in
    let zmq_iopub_port = p+1 in
    let zmq_control_port= p+2 in
    let zmq_heartbeat_port = p+3 in
    let zmq_stdin_port = p+4 in

    let command = 
        ("", Array.of_list 
        ([ 
            "iocaml.top"; 
                "-ci-shell"; string_of_int zmq_shell_port;
                "-ci-iopub"; string_of_int zmq_iopub_port;
                "-ci-control"; string_of_int zmq_control_port;
                "-ci-heartbeat"; string_of_int zmq_heartbeat_port;
                "-ci-stdin"; string_of_int zmq_stdin_port;
                "-ci-transport"; "tcp";
                "-ci-ip"; ip_addr;
        ] 
        @ (if log_file = "" then [] else [ "-log"; log_file ])
        @ (if init_file = "" then [] else [ "-init"; init_file ]))) 
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
    
let get_kernel ~zmq ~path ~notebook_guid ~ip_addr 
    ~log_file ~init_file =
    match M.kernel_of_notebook_guid notebook_guid with
    | Some(k) -> Lwt.return k
    | None -> 
        start_kernel ~zmq ~path ~notebook_guid ~ip_addr ~log_file ~init_file

let close_kernel guid =
    match M.kernel_of_kernel_guid guid with
    | None -> ()
    | Some(kernel) ->
        (* kill the kernel *)
        kernel.process#terminate;
        (* XXX close sockets? *)
        (* remove from map *)
        M.(delete_kernel guid)


