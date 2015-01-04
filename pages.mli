(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: HTML page templates
 *
 *)

val generate_notebook_html : base_path:string -> title:string -> path:string -> 
    notebook_guid:string -> 
    kernel:[`byte_code_kernel | `js_kernel of string * string | `js_kernel_file of string] -> 
    string

val generate_dashboard_html : path:string -> string

