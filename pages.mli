(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: HTML page templates
 *
 *)

val empty : Cow.Html.t

val page : string -> string -> (string -> string) -> 
    string -> string -> string -> string -> string -> 
    Cow.Html.t -> Cow.Html.t -> Cow.Html.t -> Cow.Html.t -> Cow.Html.t

val notebook_stylesheet : string -> (string -> string) -> Cow.Html.t

val notebook_header : Cow.Html.t

val notebook_site : bool -> bool -> Cow.Html.t

val notebook_scripts : (string -> string) -> string -> Cow.Html.t

val dashboard_site : string -> Cow.Html.t

val dashboard_stylesheet : (string -> string) -> Cow.Html.t

val dashboard_scripts : (string -> string) -> Cow.Html.t

val generate_notebook_html : base_path:string -> title:string -> path:string -> 
    notebook_guid:string -> kernel:string -> string

val generate_dashboard_html : path:string -> string

