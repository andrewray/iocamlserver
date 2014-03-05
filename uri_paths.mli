(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: regex's for decoding URI paths
 *
 *)

type message =
    [ `Static

    | `Root
    | `Root_guid of string
    | `Root_new
    | `Root_copy of string
    | `Root_name of string
    
    | `Notebooks
    | `Notebooks_guid of string
    | `Notebooks_checkpoint of string
    | `Notebooks_checkpoint_id of string * string

    | `Clusters

    | `Kernels
    | `Kernels_guid of string
    | `Kernels_restart of string
    | `Kernels_interrupt of string
    
    | `Error_not_found ]

type ws_message = 
    [ `Ws_shell of string
    | `Ws_iopub of string
    | `Ws_stdin of string
    | `Error_not_found ]

val decode : string -> message Lwt.t
val decode_ws : string -> ws_message Lwt.t
