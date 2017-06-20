let with_process_in cmd args f =
  let path = ["/bin";"/usr/bin"] in
  let cmd =
    List.find Sys.file_exists (List.map (fun d -> Filename.concat d cmd) path)
  in
  let ic = Unix.open_process_in (cmd^" "^args) in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn

let uname_s () =
  try
    with_process_in "uname" "-s"
      (fun ic -> Some (String.trim (input_line ic)))
  with Unix.Unix_error _ | Sys_error _ | Not_found ->
    None

let default_browser_command = "xdg-open"

let default_browser_command =
  match Sys.os_type with
  | "Unix" ->
    begin match uname_s () with
    | Some "Darwin" -> "open"
    | _ -> "xdg-open"
    end
  | _ -> "xdg-open"

