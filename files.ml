(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: Utilities to deal with notebooks on the file system
 *
 *)

module SSet = Set.Make(String)

let list_notebooks path = 
    lwt h = Lwt_unix.opendir path in
    let rec f l = 
        try_lwt 
            lwt n = Lwt_unix.readdir h in
            lwt s = Lwt_unix.stat Filename.(concat path n) in
            if Filename.check_suffix n ".ipynb" && 
               Lwt_unix.(s.st_kind = S_REG) then 
               f (n::l)
            else
                f l
        with _ ->
            Lwt.return l
    in
    f []

let new_notebook_name cur = 
    let set = List.fold_right SSet.add cur SSet.empty in
    let name = "Untitled" in
    let rec f i = 
        let name = name ^ string_of_int i in
        if SSet.mem (name ^ ".ipynb") set then
            f (i+1)
        else
            name
    in
    f 0


