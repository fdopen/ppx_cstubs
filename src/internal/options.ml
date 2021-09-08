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

let ocaml_include_dirs_default = []

let keep_tmp = ref false

let nopervasives = ref false

let use_open_struct = ref true

let verbosity = ref 1

let c_flags : string list ref = ref []

let ocaml_include_dirs : string list ref = ref ocaml_include_dirs_default

let c_output_file : string option ref = ref None

let ml_input_file : string option ref = ref None

let ml_output_file : string option ref = ref None

let toolchain : string option ref = ref None

let findlib_pkgs : string list ref = ref []

let cma_files : string list ref = ref []

let pretty = ref false

type merlin_state = {
  noperv : bool;
  uos : bool;
  verb : int;
  oflags : string list;
  tlc : string option;
  cmas : string list;
  fpkgs : string list;
}

let merlin_save () =
  {
    noperv = !nopervasives;
    uos = !use_open_struct;
    verb = !verbosity;
    oflags = !ocaml_include_dirs;
    cmas = !cma_files;
    tlc = !toolchain;
    fpkgs = !findlib_pkgs;
  }

let merlin_restore { noperv; uos; verb; oflags; cmas; tlc; fpkgs } =
  nopervasives := noperv;
  use_open_struct := uos;
  verbosity := verb;
  ocaml_include_dirs := oflags;
  cma_files := cmas;
  toolchain := tlc;
  findlib_pkgs := fpkgs

(* not yet configurable, but maybe in the future ... *)
let ocamlfind =
  match Sys.win32 with true -> "ocamlfind.exe" | false -> "ocamlfind"

type mode =
  | Regular
  | Emulate

let mode = ref Regular

let cc : string option ref = ref None

let use_cxx = ref false

let toolchain_used () =
  if !toolchain <> None then true
  else
    match Sys.getenv "OCAMLFIND_TOOLCHAIN" with
    | exception Not_found -> false
    | "" -> false
    | _ -> true

(*let reset () =
  keep_tmp := false;
  nopervasives := false;
  verbosity := 1;
  c_flags := [];
  ocaml_include_dirs := ocaml_include_dirs_default;
  c_output_file := None;
  ml_input_file := None;
  ml_output_file := None;
  toolchain := None;
  use_open_struct := true;
  findlib_pkgs := [];
  cma_files := [];
  mode := Regular;
  pretty := false;
  cc := None
*)
