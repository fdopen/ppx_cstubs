(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018-2019 fdopen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module StringMap = CCMap.Make (CCString)

let config =
  lazy
    (let buffer = Buffer.create 2048 in
     let args = [ "c"; "-config" ] in
     let args =
       match !Options.toolchain with
       | None -> args
       | Some s -> "-toolchain" :: s :: args
     in
     let stderr = if !Options.verbosity > 1 then `Stderr else `Null in
     let prog = Options.ocamlfind in
     (match Run.run prog args ~stdout:(`Buffer buffer) ~stderr with
     | exception Unix.Unix_error (e, s, _) ->
       let cmd = Run.cmd_to_string prog args in
       Printf.sprintf "Process creation \"%s\" failed with %s (%S)" cmd
         (Unix.error_message e) s
       |> failwith
     | 0 -> ()
     | x ->
       Printf.sprintf "`ocamlfind ocamlc -config` failed with %d" x |> failwith);
     let r =
       Buffer.contents buffer
       |> CCString.split_on_char '\n'
       |> CCList.filter_map (fun a ->
              match CCString.Split.left ~by:":" a with
              | None -> None
              | Some (a, b) ->
                let a = String.trim a
                and b = String.trim b in
                if a = "" then None else Some (a, b))
       |> StringMap.of_list
     in
     if r = StringMap.empty then
       failwith "invalid output of `ocamlfind ocamlc -config";
     r)

let ext_obj =
  lazy
    (if Options.(!mode = Emulate) then ".o"
    else
      match StringMap.find "ext_obj" (Lazy.force config) with
      | exception Not_found ->
        failwith "`ocamlc -config` doesn't contain ext_obj"
      | "" -> failwith "ext_obj in `ocamlc -config` is empty"
      | x -> x)

let runtime_version =
  Scanf.sscanf Sys.ocaml_version "%u.%u.%u" (fun a b c -> (a, b, c))

let version =
  lazy
    (if Options.(!mode = Emulate) then runtime_version
    else
      match StringMap.find "version" (Lazy.force config) with
      | exception Not_found ->
        if runtime_version >= (4, 3, 0) then
          failwith "`ocamlc -config` doesn't contain 'version'";
        runtime_version
      | x -> (
        try Scanf.sscanf x "%u.%u.%u" (fun a b c -> (a, b, c))
        with End_of_file | Scanf.Scan_failure _ | Failure _ ->
          failwith "`ocamlc -config` contains a surprising version string"))

let word_size =
  lazy
    (if Options.(!mode = Emulate) then Sys.word_size
    else
      match StringMap.find "word_size" (Lazy.force config) with
      | exception Not_found ->
        if runtime_version >= (4, 5, 0) then
          failwith "`ocamlc -config` doesn't contain word_size"
        else Sys.word_size
      | x -> (
        match int_of_string x with
        | exception Failure _ ->
          failwith "word_size in `ocamlc -config` is not a number"
        | (32 | 64) as x -> x
        | x ->
          Printf.sprintf "unusual word_size (%d) reported by `ocamlc -config`" x
          |> failwith))

let system =
  lazy
    (if Options.(!mode = Emulate) then "linux"
    else
      match StringMap.find "system" (Lazy.force config) with
      | exception Not_found -> failwith "`ocamlc -config` doesn't report system"
      | "" -> failwith "`ocamlc -config` reports an empty system"
      | x -> x)

let re_space = Re.Perl.re "[\r\n\t ]+" |> Re.compile

let c_compiler_flags =
  lazy
    (if Options.(!mode = Emulate) then ("gcc", [])
    else
      let c = Lazy.force config in
      match
        ( StringMap.find "c_compiler" c,
          StringMap.find "ocamlc_cflags" c,
          StringMap.find "ocamlc_cppflags" c )
      with
      | exception Not_found -> (
        if runtime_version >= (4, 6, 0) then
          failwith "`ocamlc -config` doesn't list c_compiler and flags";
        match StringMap.find "bytecomp_c_compiler" c with
        | exception Not_found ->
          failwith "`ocamlc -config` doesn't report bytecomp_c_compiler"
        | s -> (
          match Re.split re_space s with
          | [] -> failwith "`ocamlc -config` doesn't report bytecomp_c_compiler"
          | hd :: tl -> (hd, tl)))
      | "", _, _ -> failwith "`ocamlc -config` does report an empty c compiler"
      | a, b, c -> (a, Re.split re_space b @ Re.split re_space c))

let standard_library =
  lazy
    (if Options.(!mode = Emulate) then "/tmp"
    else
      match StringMap.find "standard_library" (Lazy.force config) with
      | exception Not_found ->
        failwith "`ocamlc -config` doesn't report standard_library"
      | "" -> failwith "`ocamlc -config` standard_library entry is empty"
      | x -> x)

let init () =
  (* trigger fatal errors *)
  ignore (Lazy.force word_size : int);
  ignore (Lazy.force ext_obj : string);
  ignore (Lazy.force version : int * int * int);
  ignore (Lazy.force system : string);
  ignore (Lazy.force c_compiler_flags : string * string list);
  ignore (Lazy.force standard_library : string)

let word_size () = Lazy.force word_size

let ext_obj () = Lazy.force ext_obj

let version () = Lazy.force version

let system () = Lazy.force system

let use_open_struct () = !Options.use_open_struct && version () >= (4, 8, 0)

let c_compiler_flags () = Lazy.force c_compiler_flags

let standard_library () = Lazy.force standard_library
