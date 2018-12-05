(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018 fdopen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>. *)

let dev_null = if Sys.win32 then "NUL" else "/dev/null"

module StringMap = CCMap.Make(CCString)

let config = lazy (
  CCIO.File.with_temp ?temp_dir:None ~prefix:"ppxc" ~suffix:".txt" @@ fun fln ->
  let command =
    let toolchain = match !Options.toolchain with
    | None -> ""
    | Some s -> "-toolchain " ^ Filename.quote s in
    String.concat "" [ "ocamlfind "; toolchain; " c -config > ";
                       Filename.quote fln; " 2> "; dev_null ] in
  let ec = Sys.command command in
  if ec <> 0 then
    Printf.sprintf "`ocamlfind ocamlc -config` failed with %d" ec |> failwith;
  CCIO.with_in fln @@ fun ch ->
  let r =
    CCIO.read_lines_l ch
    |> CCList.filter_map (fun a ->
      match CCString.Split.left ~by:":" a with
      | None -> None
      | Some (a,b) ->
        let a = String.trim a
        and b = String.trim b in
        if a = "" || b = "" then None
        else Some (a,b))
    |> StringMap.of_list in
  if r = StringMap.empty then
    failwith "invalid output of `ocamlfind ocamlc -config";
  r
)

let ext_obj = lazy (
  if Options.(!mode = Emulate) then ".o" else
  match StringMap.find "ext_obj" (Lazy.force config) with
  | exception Not_found -> failwith "`ocamlc -config` doesn't contain ext_obj"
  | "" -> failwith "ext_obj in `ocamlc -config` is empty"
  | x -> x )

let ocaml_version =
  Scanf.sscanf Sys.ocaml_version "%u.%u.%u" (fun a b c -> a, b, c)

let version = lazy (
  if Options.(!mode = Emulate) then ocaml_version else
  match StringMap.find "version" (Lazy.force config) with
  | exception Not_found ->
    if ocaml_version >= (4,3,0) then
      failwith "`ocamlc -config` doesn't contain 'version'";
    ocaml_version
  | x ->
    try
      Scanf.sscanf x "%u.%u.%u" (fun a b c -> a, b, c)
    with
    | End_of_file | Scanf.Scan_failure _ | Failure _ ->
      failwith "`ocamlc -config` contains a surprising version string" )

let word_size = lazy (
  if Options.(!mode = Emulate) then Sys.word_size else
  match StringMap.find "word_size" (Lazy.force config) with
  | exception Not_found ->
    if ocaml_version >= (4,5,0) then
      failwith "`ocamlc -config` doesn't contain word_size"
    else
      Sys.word_size
  | x ->
    match int_of_string x with
    | exception (Failure _) ->
      failwith "word_size in `ocamlc -config` is not a number"
    | (32|64) as x -> x
    | x ->
      Printf.sprintf "unusual word_size (%d) reported by `ocamlc -config`" x
      |> failwith )

let init () = (* trigger fatal errors *)
  ignore (Lazy.force word_size : int);
  ignore (Lazy.force ext_obj : string);
  ignore (Lazy.force version : int * int * int)

let word_size () = Lazy.force word_size
let ext_obj () = Lazy.force ext_obj
let version () = Lazy.force version
