(* 
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: regex's for decoding URI paths
 *
 *)

open Lwt
open Re

let s = char '/'
let d = char '-'
let hex = alt [digit; (no_case (rg 'a' 'f'))]
let hex n = repn hex n (Some(n))
let guid = seq [ hex 8; d; hex 4; d; hex 4; d; hex 4; d; hex 12 ]
let re_guid = compile guid

let notebooks = str "/notebooks"
let clusters = str "/clusters"
let kernels = str "/kernels"
let static = str "/static"
let root = str "/"

let re_notebooks = compile notebooks
let re_clusters = compile clusters
let re_kernels = compile kernels
let re_static = compile static
let re_root = compile root

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

let rec execl s = function
    | [] -> return `Error_not_found
    | (re,fn) :: tl ->
        return (try Some(fn (get_all (exec re s))) with _ -> None)
        >>= function
            | Some(x) -> return x
            | None -> execl s tl


let compile_decode p (re,fn) = 
    match p with 
    | None -> compile (seq re), fn
    | Some(p) -> compile (seq (p::re)), fn

let decode_root = 
    let guid = group guid in
    let re = [
        [s; str "new"; eos], (fun _ -> `Root_new);
        [s; guid; s; str "copy"; eos], (fun r -> `Root_copy(r.(1)));
        (*[s; group word; eos], (fun r -> `Root_name(r.(1)));*)
        [s; guid; eos], (fun r -> `Root_guid(r.(1)));
    ] in
    let re = List.map (compile_decode None) re in
    fun path -> execl path re

let decode_notebooks = 
    let cp, guid = str "checkpoints", group guid in
    let re = [
        [eos], (fun _ -> `Notebooks);
        [s; guid; eos], (fun r -> `Notebooks_guid(r.(1)));
        [s; guid; s; cp; eos], (fun r -> `Notebooks_checkpoint(r.(1)));
        [s; guid; s; cp; s; guid; eos], (fun r -> `Notebooks_checkpoint_id(r.(1), r.(2)));
    ] in
    let re = List.map (compile_decode (Some notebooks)) re in
    fun path -> execl path re

let decode_kernels = 
    let guid = group guid in
    let re = [
        [eos], (fun _ -> `Kernels);
        [s; guid; eos], (fun r -> `Kernels_guid(r.(1)));
        [s; guid; s; str "restart"; eos], (fun r -> `Kernels_restart(r.(1)));
        [s; guid; s; str "interrupt"; eos], (fun r -> `Kernels_interrupt(r.(1)));
    ] in
    let re = List.map (compile_decode (Some kernels)) re in
    fun path -> execl path re

let decode_ws = 
    let guid = group guid in
    let re = [
        [s; guid; s; str "shell"; eos], (fun r -> `Ws_shell(r.(1)));
        [s; guid; s; str "iopub"; eos], (fun r -> `Ws_iopub(r.(1)));
        [s; guid; s; str "stdin"; eos], (fun r -> `Ws_stdin(r.(1)));
    ] in
    let re = List.map (compile_decode (Some kernels)) re in
    fun path -> execl path re

let decode path = 
    let d = [
        (fun p -> p="" || p="/"), (fun _ -> return `Root);
        execp re_static, (fun _ -> return `Static);
        execp re_notebooks, decode_notebooks;
        execp re_kernels, decode_kernels;
        execp re_clusters, (fun _ -> return `Clusters);
        execp re_root, decode_root;
    ] in
    let rec check = function
        | [] -> return `Error_not_found
        | (m,v)::tl ->
            return (m path) 
            >>= fun m -> if m then v path else check tl
    in
    check d

