(*
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: Utilities to deal with notebooks on the file system
 *
 *)
open Lwt.Infix

module SSet = Set.Make(String)

let list_notebooks path =
  Lwt_unix.opendir path >>= fun h ->
  let rec f l =
    Lwt.catch (fun () ->
        Lwt_unix.readdir h >>= fun n ->
        Lwt_unix.stat Filename.(concat path n) >>= fun s ->
        if Filename.check_suffix n ".ipynb" &&
           Lwt_unix.(s.st_kind = S_REG) then
          f (Filename.(chop_suffix (basename n) ".ipynb")::l)
        else
          f l
      ) (fun _ -> Lwt.return l)
  in
  f []

let new_notebook_name cur =
  let set = List.fold_right SSet.add cur SSet.empty in
  let name = "Untitled" in
  let rec f i =
    let name = name ^ string_of_int i in
    Lwt.return (SSet.mem name set)
    >>= function
    | true -> f (i+1)
    | false -> Lwt.return name
  in
  f 0

(* the json that's served for an empty notebook *)
let empty_notebook title =
    let open Yojson.Basic in
    pretty_to_string ~std:true
        (`Assoc [
            "metadata", `Assoc [ "language", `String "ocaml"; "name", `String title; ];
            "nbformat", `Int 3; "nbformat_minor", `Int 0;
            "worksheets", `List [ `Assoc [ "cells", `List []; "metadata", `Assoc []; ]; ];
        ])

let is_regular_file f = try Unix.((stat f).st_kind = S_REG) with _ -> false
let using_out n f = let n = open_out n in let r = f n in close_out n; r

let file_or_path file_or_path =
    let failwith x = failwith (file_or_path ^ ": " ^ x) in
    let file_or_path =
        if file_or_path = "" || file_or_path = "." || file_or_path = "./" then
            Unix.getcwd ()
        else if Filename.is_relative file_or_path then
            Filename.concat (Unix.getcwd ()) file_or_path
        else
            file_or_path
    in

    if Filename.check_suffix file_or_path ".ipynb" then
        (* if the name ends in .ipynb then assume it is a file *)
        let split s = Filename.(dirname s, basename s) in
        if Sys.file_exists file_or_path then
            if is_regular_file file_or_path then split file_or_path
            else failwith "expecting a file"
        else
            let path,name = split file_or_path in
            let () = using_out file_or_path
                (fun file ->
                    output_string file (empty_notebook Filename.(chop_suffix name ".ipynb")))
            in
            path,name
    else
        (* otherwise a directory *)
        if Sys.file_exists file_or_path then
            if Sys.is_directory file_or_path then
                file_or_path, ""
            else
                failwith "expecting a directory"
        else
            failwith "directory doesnt exist"

let replace_dict k v = function
    | `Assoc(l) -> `Assoc ((k,v) :: (List.remove_assoc k l))
    | _ -> failwith "not a json dict"

let rejoin = function
    | `String s -> `String s
    | `List l -> `String (String.concat "" (List.map Yojson.Basic.Util.to_string l))
    | _ as x -> failwith ("rejoin: expecting string or list" ^ Yojson.Basic.pretty_to_string x)

let split = function
  | `List l -> `List l
  | `String s -> begin
    let split str =
      let len = String.length str in
      let rec scan pos =
        if pos = (len-1) then pos
        else if str.[pos] = '\n' then pos
        else scan (pos+1)
      in
      let rec split start_pos =
        if start_pos >= len then []
        else
          let end_pos = scan start_pos in
          (start_pos,end_pos) :: split (end_pos+1)
      in
      List.map (fun (s,e) -> String.sub str s (e-s+1)) (split 0)
    in
    `List (List.map (fun s -> `String s) (split s))
  end
  | _ as x -> failwith ("split: expecting string or list" ^ Yojson.Basic.pretty_to_string x)

let process_lines fn json =
    let open Yojson.Basic in

    let failwith message json =
        failwith (message ^ " : " ^ pretty_to_string json)
    in

    let map_dict name json f =
        let open Yojson.Basic in
        let el = Util.member name json in
        if el = `Null then json
        else replace_dict name (f el) json
    in
    let map_dict_list_el name json f =
        map_dict name json
            (function
                | `List l -> `List (List.map f l)
                | _ as x -> failwith ("map_dict_list_el: expecting list in " ^ name) x)
    in

    let outputs json =
        List.fold_left
            (fun json name -> map_dict name json fn)
            json [ "text"; "html"; "svg"; "latex"; "javascript"; "json" ]
    in
    let cell json =
        match Util.member "cell_type" json with
        | `String "code" ->
            (* rewrite "input" and "outputs" *)
            let json = map_dict "input" json fn in
            map_dict_list_el "outputs" json outputs
        | `String _ ->
            map_dict "source" json fn
        | _ as x -> failwith "invalid cell type" x
    in
    let worksheet json = map_dict_list_el "cells" json cell in
    map_dict_list_el "worksheets" json worksheet

let diffable_pretty_to_string json =
  let open Easy_format in
  let rec f = function
    | List(("[", s, c, p), t) ->
        List(("[", s, c, {p with wrap_body = `Force_breaks}), List.map f t)
    | List((o, s, c, p), t) -> List((o, s, c, p), List.map f t)
    | Label((t0, p), t1) -> Label((f t0, p), f t1)
    | _ as x -> x
  in
  Pretty.to_string (f (Yojson.Basic.pretty_format ~std:true json))

let prepare_ipynb_for_saving no_split_lines data =
    let open Yojson.Basic in
    let json = from_string data in

    let metadata = Util.member "metadata" json in
    let name = Util.member "name" metadata in
    let filename = Util.to_string name in

    (* rewrite the json with an empty notebook name *)
    let json = replace_dict "metadata" (replace_dict "name" (`String "") metadata) json in
    let json =
      if no_split_lines then to_string ~std:true json
      else diffable_pretty_to_string (process_lines split json)
    in

    filename, json

let load_ipynb_for_serving path nbname =
    let open Yojson.Basic in
    Lwt_io.(with_file ~mode:input (path (nbname ^ ".ipynb")) read) >>= fun data ->
    let json = from_string data in
    let metadata = Util.member "metadata" json in

    let json = replace_dict "metadata" (replace_dict "name" (`String nbname) metadata) json in
    let json = process_lines rejoin json in

    Lwt.return (to_string ~std:true json)


let tutorial_notebook () =
  let open Yojson.Basic in
  match Tutorial.read "tutorial.ipynb" with
  | None -> failwith "tutorial not found"
  | Some(data) ->
    let json = from_string data in
    let metadata = Util.member "metadata" json in

    let json = replace_dict "metadata" (replace_dict "name" (`String "tutorial") metadata) json in
    let json = process_lines rejoin json in

    to_string ~std:true json

(*************************************************************)
(* static site generation *)

let paths fln =
  let rec paths lst fln =
    let dir = Filename.dirname fln in
    match lst with
    | prev_dir :: tl when prev_dir = dir -> lst
    | _ -> paths (dir :: lst) dir
  in
  paths [fln] fln

let create_dir_for_file to_file =
  let to_dir = Filename.dirname to_file in
  let to_paths = paths to_dir in
  let create_dir d =
    try
      if Sys.is_directory d then ()
      else failwith ("not a directory: " ^ d)
    with Sys_error _ -> begin
      try Unix.mkdir d 0o777
      with _ -> failwith ("couldn't make directory: " ^ d)
    end
  in
  List.iter create_dir to_paths

let write_file data to_file =
  let f = open_out_bin to_file in
  output_string f data;
  close_out f

let copy_static to_dir =
  let files = Filesys.file_list in
  let write_static_file from_file =
    let to_file = Filename.concat to_dir from_file in
    (* create dir if it doesn't exist already *)
    create_dir_for_file to_file;
    (match Filesys.read from_file with
    | None -> failwith ("couldn't find file in filesys: " ^ from_file)
    | Some(data) -> write_file data to_file)
  in
  let () = Printf.printf "copying static files...%!" in
  let () = List.iter write_static_file files in
  let () = Printf.printf "ok\n%!" in
  ()

let copy_js_kernel to_dir iocaml_kernel =
  let mk_kernel_name t = "kernel." ^ t ^ ".js" in
  let spath = "static/services/kernels/js" in
  let in_file =
    match iocaml_kernel with
    | `byte_code_kernel -> failwith "you must specify a javascript kernel"
    | `js_kernel(p, t) -> Filename.concat p (Filename.concat spath (mk_kernel_name t))
    | `js_kernel_file f -> f
  in
  let out_file =
    match iocaml_kernel with
    | `byte_code_kernel -> failwith "you must specify a javascript kernel"
    | `js_kernel(p, t) -> Filename.concat to_dir (Filename.concat spath (mk_kernel_name t))
    | `js_kernel_file(f) -> Filename.concat to_dir (Filename.concat spath "kernel.js")
  in
  let () = Printf.printf "copying kernel '%s'...%!" in_file in
  let in_file = open_in in_file in
  let out_file = open_out out_file in
  let buf = Bytes.create 1024 in
  let rec copy () =
    let len = input in_file buf 0 1024 in
    if len=0 then ()
    else begin
      output out_file buf 0 len;
      copy ()
    end
  in
  let () = copy () in
  let () = Printf.printf "ok\n%!" in
  ()

let get_notebook_list notebook_path filename =
  let () = Printf.printf "getting notebook list...%!" in
  let nb =
    if filename <> "" then
      (* just the 1 notebook, as specified on the commandline.  check it exists *)
      let notebook_filename = Filename.concat notebook_path filename in
      if Sys.file_exists notebook_filename then [notebook_filename]
      else failwith ("notebook doesn't exist: " ^ notebook_filename)
    else
      (* find all notebooks in given directory *)
      let dirh = Unix.opendir notebook_path in
      let rec f () =
        match (try Some( Unix.readdir dirh ) with _ -> None) with
        | None -> []
        | Some(x) -> (Filename.concat notebook_path x) :: f ()
      in
      let files = f () in
      let () = Unix.closedir dirh in
      let notebook_filenames = List.filter (fun s -> Filename.check_suffix s ".ipynb") files in
      match notebook_filenames with
      | [] -> failwith ("no notebooks in: " ^ notebook_path)
      | _ -> notebook_filenames
  in
  let () = Printf.printf "ok\n%!" in
  nb

let create_notebook_html to_dir base_path js_kernel notebook_name =
  let () = Printf.printf "creating html for %s...%!" notebook_name in
  let path = base_path ^ "/notebooks" in
  let notebook_guid = Filename.basename notebook_name in
  let html = Pages.generate_notebook_html
    ~base_path ~title:"IOCaml-Notebook" ~path ~notebook_guid ~kernel:js_kernel
  in
  let html_file_name =
    let html_file = Filename.(chop_suffix notebook_guid ".ipynb") ^ ".html" in
    Filename.(concat to_dir html_file)
  in
  let f = open_out html_file_name in
  let () = output_string f html in
  let () = close_out f in
  let () = Printf.printf "ok\n%!" in
  ()

let copy_ipynb to_dir notebook_name =
  let () = Printf.printf "copying notebook %s...%!" notebook_name in
  (* load the notebook (into servable format) *)
  let notebook_dir = Filename.dirname notebook_name in
  let notebook_file_name = Filename.basename notebook_name in
  let notebook =
    let file = Filename.(chop_suffix notebook_file_name ".ipynb") in
    load_ipynb_for_serving (Filename.concat notebook_dir) file in
  notebook >>= fun notebook ->
  (* save the notebook *)
  let output_notebook_file_name =
    Filename.(concat to_dir (concat "notebooks" notebook_file_name))
  in
  let () = create_dir_for_file output_notebook_file_name in
  let f = open_out output_notebook_file_name in
  let () = output_string f notebook in
  let () = close_out f in
  let () = Printf.printf "ok\n%!" in
  Lwt.return ()

let create_static_site
  ~to_dir ~notebook_path ~notebook_filename
  ~iocaml_kernel
  ~base_path =
  let () = copy_static to_dir in
  let () = copy_js_kernel to_dir iocaml_kernel in
  let notebooks = get_notebook_list notebook_path notebook_filename in
  let () = List.iter (create_notebook_html to_dir base_path iocaml_kernel) notebooks in
  Lwt_list.iter_s (copy_ipynb to_dir) notebooks
