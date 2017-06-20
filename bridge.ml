(*
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: bridge websockets and zmq sockets
 *
 *)
open Lwt.Infix
open Iocaml_zmq

type ws_stream = Websocket_lwt.Frame.t Lwt_stream.t
type ws_push = Websocket_lwt.Frame.t -> unit Lwt.t
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

let ws_to_zmq verbose name stream socket =
  Lwt_stream.next stream >>= fun frame ->
  let data = frame.Websocket_lwt.Frame.content in
  (if verbose > 1 then Lwt_io.eprintf "[ws->zmq]%s: %s\n" name data
   else Lwt.return_unit) >>= fun () ->
  Lwt.catch
    (fun () -> Lwt_zmq.Socket.send_all socket (zmq_of_ws_message data))
    (fun _ -> Lwt.return_unit)

let zmq_to_ws verbose name socket (push : ws_push) =
  Lwt_zmq.Socket.recv_all socket >>= fun frames ->
  (if verbose > 1 then
     Lwt_list.iter_s (Lwt_io.eprintf "[zmq->ws]%s: %s\n" name) frames
   else
     Lwt.return ()) >>= fun () ->
  Lwt.catch (fun () ->
      let frame = ws_of_zmq_message frames in
      push (Websocket_lwt.Frame.create ~content:frame ())
    ) (fun _exn -> Lwt.return_unit)

let rec ws_zmq_comms verbose name socket uri (stream, push) =
  zmq_to_ws verbose name socket push <?> ws_to_zmq verbose name stream socket >>= fun _ ->
  ws_zmq_comms verbose name socket uri (stream,push)

let ws_init verbose (client : Websocket_lwt.Connected_client.t) =
  let open Websocket_lwt in
  let open Kernel in
  Lwt.catch (fun () ->
      Connected_client.recv client >>= fun cookie ->
      (* we get one special message per channel, after which it's comms time *)
      let cookie = cookie.Frame.content in
      (if verbose > 1 then
         Lwt_io.eprintf "cookie:[%i] %s\n" (String.length cookie) cookie
       else
         Lwt.return_unit) >>= fun () ->
      (* parse the uri to find out which socket we want *)
      let get guid =
        match M.kernel_of_kernel_guid guid with
        | None -> Lwt.fail_with ("cant find kernel: " ^ guid)
        | Some x -> Lwt.return x
      in
      let uri = Cohttp.Request.uri (Connected_client.http_request client) in
      let stream =
        Websocket_lwt.mk_frame_stream
          (fun () -> Connected_client.recv client) in
      let send = Connected_client.send client in
      Uri_paths.decode_ws (Uri.path uri) >>= function
      | `Ws_shell(guid) ->
        get guid >>= fun k -> ws_zmq_comms verbose "shell" (k.shell()) uri (stream, send)
      | `Ws_stdin(guid) ->
        get guid >>= fun k -> ws_zmq_comms verbose "stdin" (k.stdin()) uri (stream, send)
      | `Ws_iopub(guid) ->
        get guid >>= fun k -> ws_zmq_comms verbose "iopub" (k.iopub()) uri (stream, send)
      | `Error_not_found ->
        Lwt.fail (Failure "invalid websocket url")
    ) (fun x ->
        if verbose > 0 then
          Lwt_io.eprintf "ws_init failed with %s\n" (Printexc.to_string x)
        else
          Lwt.return_unit)
