(*
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: HTTP server + Kernel control
 *
 *)

(* see wiki for kernel messages *)

open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Iocaml_zmq

let address = ref "127.0.0.1"
let verbose = ref 0
let file_or_path = ref ""

let tutorial = ref false

let static_file_path = ref ""
let serve_uri_path = ref []
let serve_file_path = ref []

let iocamljs_kernel = ref ""

let browser = ref Config.default_browser_command

let () = Findlib.init ()
let configure_js_serve () =
  let stdlib = Findlib.ocaml_stdlib () in
  let findlib = Findlib.default_location () in
  (* mapping for compiler and findlib *)
  serve_uri_path :=
    stdlib ::
    findlib ::
    !serve_uri_path;
  serve_file_path :=
    stdlib ::
    findlib ::
    !serve_file_path

let no_split_lines = ref false

let static_site_path = ref ""
let static_site_base_path = ref ""

let () =
  Arg.(parse (align [
      "-tutorial", Set(tutorial), " show IOCaml tutorial notebook";
      "-ip", Set_string(address), "<ip-address> ip address of server";
      "-js", Set_string(iocamljs_kernel), "<kernel> use iocamljs kernel";
      "-static", Set_string(static_file_path), "<dir> serve static files from dir";
      "-serve", String(fun s -> serve_uri_path := s :: !serve_uri_path;
                        serve_file_path := s :: !serve_file_path),
      "<uri+path> serve files from same path and uri";
      "-serve-at", Tuple([ String(fun s -> serve_uri_path := s :: !serve_uri_path);
                           String(fun s -> serve_file_path := s :: !serve_file_path) ]),
      " <uri> <path> serve files from path on uri";
      "-serve-jslibs", Unit(configure_js_serve),
      " configure paths to serve libraries for js kernel";
      "-log", Set_string(Kernel.(kernel_args.log_file)), "<file> kernel log file";
      "-init", Set_string(Kernel.(kernel_args.init_file)), "<file> kernel init file";
      "-completion", Set(Kernel.(kernel_args.completion)), " enable tab completion";
      "-object-info", Set(Kernel.(kernel_args.object_info)), " enable introspection";
      "-browser", Set_string(browser), "<exe> browser command [xdg-open]";
      "-no-split-lines", Set(no_split_lines), " dont split lines when saving";
      "-create-static-site", Set_string(static_site_path),
      " <output path> create site for static serving (ie gh-pages)";
      "-static-site-base-path", Set_string(static_site_base_path),
      " <base path> set static site base path";
      "-v", Unit(fun () -> incr verbose), " increase verbosity";
    ])
      (fun s -> file_or_path := s)
      "iocaml [options] [file-or-path]")

let notebook_path, file_to_open =
  if !tutorial then begin
    let name = "iocaml_tutorial.ipynb" in
    let _ = Files.using_out name
        (fun file -> output_string file (Files.tutorial_notebook ()))
    in
    "./", name
  end else
    Files.file_or_path !file_or_path

let filename name = Filename.(concat notebook_path name)

let serve_files = List.rev (List.map2 (fun a b -> a,b) !serve_uri_path !serve_file_path)

let share_dir () =
  try
    let ic = Unix.open_process_in ("opam config var share 2>/dev/null") in
    let r = input_line ic in
    let r =
      let len = String.length r in
      if len>0 && r.[len - 1] = '\r' then String.sub r 0 (len-1) else r
    in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> r
    | _ -> failwith ""
  with
  | _ -> failwith ("could not query opam for share directory")

let iocaml_kernel =
  match !iocamljs_kernel with
  (* standard byte code kernel, communicated over websockets.  uses kernel.js from ipython *)
  | "" -> `byte_code_kernel
  (* direct file link to javscript kernel *)
  | k when Sys.file_exists k && Filename.check_suffix k ".js" -> `js_kernel_file(k)
  (* javascript kernel loaded from std install dir *)
  | k -> `js_kernel(share_dir() ^ "/iocamljs-kernel/profile", k)

let () =
  if !verbose > 0 then begin
    let open Printf in
    printf "ip address: '%s'\n" !address;
    printf "notebook path: '%s'\n" notebook_path;
    printf "file to open: '%s'\n" file_to_open;
    printf "extra static dir: '%s'\n" !static_file_path;
    List.iter (fun (u,p) ->
        printf "serve uri: '%s' -> '%s'\n" u p) serve_files;
    (match iocaml_kernel with
     | `byte_code_kernel -> printf "kernel: byte code\n"
     | `js_kernel_file(f) -> printf "kernel: javascript file %s\n" f
     | `js_kernel(p,t) -> printf "kernel: javscript %s @ %s\n" t p);
    flush stdout;
  end

(* zmq initialization *)
let zmq = ZMQ.Context.create ()

let header typ =
  let h = Header.init () in
  let h = Header.add h "Content-Type" typ in
  let h = Header.add h "Server" "iocaml" in
  h

let header_none = Header.init ()
let header_html = header "text/html; charset=UTF-8"
let header_css = header "text/css"
let header_javascript = header "application/javascript"
let header_json = header "application/json"
let header_font = header "application/x-font-woff"
let header_redirect guid =
  let h = header_html in
  let h = Header.add h "Location" ("/" ^ guid) in
  h
let header_date h =
  let day = function
    | 0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed"
    | 4 -> "Thu" | 5 -> "Fri" | _ -> "Sat"
  in
  let month = function
    | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr"
    | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul" | 7 -> "Aug"
    | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | _ ->"Dec"
  in
  let tm = Unix.(gmtime (gettimeofday ())) in
  let tm = Unix.(Printf.sprintf "%s, %.2i %s %.4i %.2i:%.2i:%.2i GMT"
                   (day tm.tm_wday) tm.tm_mday (month tm.tm_mon) (tm.tm_year+1900) tm.tm_hour tm.tm_min tm.tm_sec)
  in
  let h = Header.add h "Date" tm in
  h
let header_binary = header "application/octet-stream"
let header_plain_user_charset = header "text/plain; charset=x-user-defined"

let checkpoint_date () =
  let tm = Unix.(gmtime (gettimeofday ())) in
  Unix.(Printf.sprintf "%.4i-%.2i-%.2iT%.2i:%.2i:%.2i.000000+00:00"
          (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday
          tm.tm_hour tm.tm_min tm.tm_sec)

let header_of_extension filename =
  if Filename.check_suffix filename ".js" then header_javascript
  else if Filename.check_suffix filename ".css" then header_css
  else if Filename.check_suffix filename ".ipynb" then header_json
  else if Filename.check_suffix filename ".woff" then header_font
  else header_none

let kernel_id_json ~kernel_guid ~address ~ws_port =
  let open Yojson.Basic in
  to_string ~std:true
    (`Assoc [
        "kernel_id", `String kernel_guid;
        "ws_url", `String ("ws://" ^ address ^ ":" ^ string_of_int ws_port);
      ])

let not_found () =
  (if !verbose > 0 then
     Lwt_io.eprintf "Not_found\n"
   else
     return ())
  >>= fun () -> Server.respond_not_found ()

let notebook_list notebook_path =
  Files.list_notebooks notebook_path >>= fun l ->
  let open Yojson.Basic in
  let json nb =
    let notebook_guid = Kernel.M.notebook_guid_of_filename nb in
    `Assoc [
      "kernel_id", (* check if kernel is already running *)
      (match Kernel.M.kernel_of_notebook_guid notebook_guid with
       | None -> `Null
       | Some(x) -> `String (Kernel.M.kernel_guid_of_kernel x));
      "name", `String nb;
      "notebook_id", `String notebook_guid;
    ]
  in
  let json = `List (List.map json l) in
  Server.respond_string ~status:`OK ~body:(to_string ~std:true json) ()

(* read notebook from file *)
let send_notebook guid =
  Lwt.catch (fun () ->
      let name =
        try return (Kernel.M.filename_of_notebook_guid guid)
        with _ -> fail (Failure "bad_file")
      in
      name >>= fun name ->
      Lwt_io.(with_file ~mode:input (filename (name ^ ".ipynb")) read) >>= fun notebook ->
      Kernel.M.dump_state !verbose;
      Server.respond_string ~status:`OK ~body:notebook ()
    ) (fun _ -> not_found ())

let register_notebooks notebook_path =
  Files.list_notebooks notebook_path >>= fun l ->
  Lwt_list.iter_s
    (fun nb -> return (ignore (Kernel.M.notebook_guid_of_filename nb))) l

let serve_crunched_files uri =
  (* serve from crunched file system  *)
  let fname = Server.resolve_file ~docroot:"" ~uri:uri in
  (match Filesys.read fname with
   | None -> not_found()
   | Some(x) ->
     Server.respond_string ~status:`OK ~headers:(header_of_extension fname) ~body:x ())

let serve_from uri path next =
  if path <> "" then
    let fname = Server.resolve_file ~docroot:path ~uri:uri in
    if Sys.file_exists fname then
      (if !verbose > 0 then
         Lwt_io.eprintf "  [  STATIC]: %s [%s] [%s]\n" fname path (Uri.path uri)
       else
         return ()
      ) >>= fun () ->
      Server.respond_file ~headers:(header_of_extension fname) ~fname:fname ()
    else
      next ()
  else
    next ()

let serve_static_files uri =
  let serve_from = serve_from uri in
  serve_from !static_file_path (fun () ->
      match iocaml_kernel with
      | `byte_code_kernel -> serve_crunched_files uri
      | `js_kernel(path, _) -> serve_from path (fun () -> serve_crunched_files uri)
      | `js_kernel_file(fname) -> (* XXX this wont serve the custom icon I think XXX not hugely important, but to be fixed! *)
        if Uri.path uri = "/static/services/kernels/js/kernel.js" then begin
          (if !verbose > 0 then
             Lwt_io.eprintf "  [JSKERNEL]: %s [%s]\n" fname (Uri.path uri)
           else
             return ()) >>= fun () ->
          Server.respond_file ~headers:(header_of_extension fname) ~fname:fname ()
        end else serve_crunched_files uri
    )

let save_notebook guid body =
  let old_filename = Kernel.M.filename_of_notebook_guid guid in
  (*lwt new_filename = get_filename_of_ipynb body in*)
  let new_filename, body = Files.prepare_ipynb_for_saving !no_split_lines body in
  (Lwt_io.(with_file ~mode:output
             (filename (new_filename ^ ".ipynb"))
             (fun f -> write f body))) >>= fun () ->
  let () =
    if new_filename <> old_filename then
      Kernel.M.change_filename old_filename new_filename guid
  in
  Kernel.M.dump_state !verbose;
  Server.respond_string ~status:`OK ~headers:(header_date header_html) ~body:"" ()


let http_server address port ws_port notebook_path =
  let decode = Uri_paths.decode serve_files in

  let callback (_,conn_id) req body =
    let uri = Request.uri req in
    let meth = Request.meth req in
    let path = Uri.path uri in

    decode path >>= fun decode ->

    (* XXX log all messages that are not just serving notebook files *)
    (if (!verbose > 0 && decode <> `Static) || (!verbose > 1) then
       Lwt_io.eprintf "%s [%8s]: [%s] %s -> %s\n%!"
         (Connection.to_string conn_id)
         (Code.string_of_method meth)
         (Uri_paths.string_of_message decode)
         (Uri.to_string uri) path
     else
       return ()) >>= fun () ->

    let query_param var =
      match Uri.get_query_param uri var with
      | None -> Lwt.fail (Failure ("failed to get param: " ^ var))
      | Some(x) -> return x
    in

    match decode with

    | `Root ->
      let dashboard = Pages.generate_dashboard_html ~path:notebook_path in
      Server.respond_string ~status:`OK ~headers:header_html ~body:dashboard ()

    | `Static ->
      serve_static_files uri

    | `File(fname) ->
      (if !verbose > 0 then
         Lwt_io.eprintf "  [    DATA] %s\n" fname
       else
         return ()) >>= fun () ->
      Server.respond_file ~headers:header_plain_user_charset ~fname:fname ()

    | `Root_guid(guid) ->
      let notebook = Pages.generate_notebook_html
          ~base_path:""
          ~title:"IOCaml-Notebook" ~path:notebook_path
          ~notebook_guid:guid ~kernel:iocaml_kernel
      in
      Kernel.M.dump_state !verbose;
      Server.respond_string ~status:`OK ~headers:header_html ~body:notebook ()

    | `Root_new ->
      (* create new .ipynb file *)
      Files.(list_notebooks notebook_path >>= new_notebook_name) >>= fun name ->
      (Lwt_io.(with_file ~mode:output
                 (filename (name ^ ".ipynb"))
                 (fun f -> write f (Files.empty_notebook name))))
      >>= fun () ->
      let guid = Kernel.M.notebook_guid_of_filename name in
      (* 302 Found, redirect to `Root_guid *)
      Server.respond_string ~status:`Found ~headers:(header_redirect guid) ~body:"" ()

    | `Root_name(name) ->
      Lwt.catch (fun () ->
          Server.respond_string ~status:`Found
            ~headers:(header_redirect (Kernel.M.notebook_guid_of_filename name))
            ~body:"" ()
        ) (fun _ -> not_found ())

    | `Root_copy(guid) -> not_found ()

    | `Notebooks -> notebook_list notebook_path

    | `Notebooks_guid(guid) when meth = `GET ->
      Lwt.catch (fun () ->
          (* read notebook from file *)
          let name =
            try return (Kernel.M.filename_of_notebook_guid guid)
            with _ -> fail (Failure "bad_file") in
          name >>= fun name ->
          Files.load_ipynb_for_serving filename name >>= fun notebook ->
          Kernel.M.dump_state !verbose;
          Server.respond_string ~status:`OK ~body:notebook ()
        ) (fun _ -> not_found ())

    | `Notebooks_guid(guid) when meth = `PUT ->
      (* save notebook *)
      Lwt.catch (fun () ->
          Cohttp_lwt_body.to_string body >>= fun body ->
          Kernel.M.dump_state !verbose;
          save_notebook guid body
        ) (fun _ -> not_found ())

    | `Notebooks_checkpoint(_) ->
      Server.respond_string ~status:`OK ~body:"[]" ()

    | `Notebooks_checkpoint_id(_) -> not_found ()

    | `Clusters ->
      Server.respond_string ~status:`OK ~body:"[]" ()

    | `Kernels when meth = `POST ->
      Lwt.catch (fun () ->
          query_param "notebook" >>= fun notebook_guid ->
          Kernel.get_kernel ~zmq ~path:notebook_path ~notebook_guid ~ip_addr:address
          >>= fun kernel ->
          Kernel.M.dump_state !verbose;
          Server.respond_string ~status:`OK
            ~body:(kernel_id_json ~kernel_guid:kernel.Kernel.guid ~address ~ws_port) ()
        ) (fun _ -> not_found ())

    | `Kernels_guid(guid) when meth = `DELETE ->
      let () = Kernel.close_kernel guid in
      Kernel.M.dump_state !verbose;
      Server.respond_string ~status:`OK ~body:"" ()

    | `Kernels_restart(guid) ->
      Lwt.catch (fun () ->
          (* stop kernel *)
          let () = Kernel.close_kernel guid in
          (* re-start kernel *)
          let notebook_guid = Kernel.M.notebook_guid_of_kernel_guid guid in
          Kernel.get_kernel ~zmq ~path:notebook_path ~notebook_guid ~ip_addr:address
          >>= fun kernel ->
          Kernel.M.dump_state !verbose;
          Server.respond_string ~status:`OK
            ~body:(kernel_id_json ~kernel_guid:kernel.Kernel.guid ~address ~ws_port) ()
        ) (fun _ -> not_found ())

    | `Kernels_interrupt(guid) ->
      (match Kernel.M.kernel_of_kernel_guid guid with
       | Some(kernel) ->
         kernel.Kernel.process#kill Sys.sigint; (* interrupt *)
         Kernel.M.dump_state !verbose;
         Server.respond_string ~status:`OK ~body:"" ()
       | None -> not_found ())

    | `Error_not_found | _ -> not_found ()
  in
  (*let conn_closed conn_id () = () in
    let config = { Server.callback; conn_closed } in
    Server.create ~address ~port config*)
  let conn_closed (_,_) = () in
  Conduit_lwt_unix.init ~src:address () >>= fun ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx () in
  let mode = `TCP (`Port port) in
  let config = Cohttp_lwt_unix.Server.make ~callback ~conn_closed () in
  Cohttp_lwt_unix.Server.create ~ctx ~mode config

let run_servers address notebook_path =
  register_notebooks notebook_path >>= fun () ->

  (* find ports for http and websocket servers *)
  let rec find_port_pair port =
    Kernel.n_ports_available address port 2 >>= fun ok ->
    if ok then return port
    else find_port_pair (port+2)
  in
  find_port_pair 8888 >>= fun http_port ->
  let ws_port = http_port + 1 in
  let () = Printf.printf "[iocaml] listening on %s:%d\n%!" address http_port in
  (*if !verbose > 0 then begin
    Printf.printf "listening for HTTP on port: %d\n%!" http_port;
    Printf.printf "listening for websockets on port: %d\n%!" ws_port;
    end;*)

  (* http server *)
  let http_server = http_server address http_port ws_port notebook_path in

  (* websocket server *)
  let ws_server =
    let uri = Uri.of_string ("http://" ^ address ^ ":" ^ string_of_int ws_port) in
    Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
    Conduit_lwt_unix.(
      endp_to_server ~ctx:default_ctx endp >>= fun server ->
      Websocket_lwt.establish_standard_server ~ctx:default_ctx ~mode:server
        (Bridge.ws_init !verbose)
    )
  in

  (* start webbrowser, what about mac-osx? 'open'? *)
  let browser_command =
    if file_to_open <> "" then
      let guid =
        Kernel.M.notebook_guid_of_filename
          (Filename.(chop_suffix file_to_open ".ipynb"))
      in
      ("", [| !browser; "http://" ^ address ^ ":" ^
                        string_of_int http_port ^ "/" ^ guid |])
    else
      ("", [| !browser; "http://" ^ address ^ ":" ^ string_of_int http_port |])
  in
  let _ = Lwt_process.open_process_none browser_command in

  Lwt.join [http_server; ws_server]

let close_kernels () =
  (* kill all running kernels *)
  Kernel.M.iter_kernels
    (fun _ v -> v.Kernel.process#terminate)

let run_iocaml_server () =
  Sys.catch_break true;
  try
    (*at_exit close_kernels;*)
    Lwt_unix.run (run_servers !address notebook_path)
  with
  | Sys.Break -> begin
      close_kernels ();
      (*ZMQ.term zmq*)
    end

let () =
  if !static_site_path = "" then
    run_iocaml_server ()
  else
    Lwt_unix.run
      (Files.create_static_site
         ~to_dir:!static_site_path
         ~notebook_path ~notebook_filename:file_to_open
         ~iocaml_kernel:iocaml_kernel
         ~base_path:!static_site_base_path)
