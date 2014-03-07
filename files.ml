(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: Utilities to deal with notebooks on the file system
 *
 *)
open Lwt

module SSet = Set.Make(String)

let list_notebooks path = 
    lwt h = Lwt_unix.opendir path in
    let rec f l = 
        try_lwt 
            lwt n = Lwt_unix.readdir h in
            lwt s = Lwt_unix.stat Filename.(concat path n) in
            if Filename.check_suffix n ".ipynb" && 
               Lwt_unix.(s.st_kind = S_REG) then 
               f (Filename.(chop_suffix (basename n) ".ipynb")::l)
            else
                f l
        with _ ->
            return l
    in
    f []

let new_notebook_name cur = 
    let set = List.fold_right SSet.add cur SSet.empty in
    let name = "Untitled" in
    let rec f i = 
        let name = name ^ string_of_int i in
            return (SSet.mem name set)
        >>= function
            | true -> f (i+1)
            | false -> return name
    in
    f 0

(* the json that's served for an empty notebook *)
let empty_notebook title = 
    let open Yojson.Basic in
    to_string 
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
        (* it the name ends in .ipynb then assume it is a file *)
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

