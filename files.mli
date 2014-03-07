(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: Utilities to deal with notebooks on the file system
 *
 *)

(** list notebooks in the given path *)
val list_notebooks : string -> string list Lwt.t

(** get name for a new notebook, given list of current notebooks *)
val new_notebook_name : string list -> string Lwt.t

val empty_notebook : string -> string

val file_or_path : string -> string * string
