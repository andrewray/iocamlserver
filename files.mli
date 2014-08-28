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

(** extract notebook name from json, then remove it *)
val prepare_ipynb_for_saving : bool -> string -> string * string

val load_ipynb_for_serving : (string -> string) -> string -> string Lwt.t

