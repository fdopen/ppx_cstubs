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

let ocaml_flags_default = ["-package";"ctypes"]
let keep_tmp = ref false
let verbosity = ref 0
let c_flags : string list ref = ref []
let ocaml_flags : string list ref = ref ocaml_flags_default
let c_output_file : string option ref = ref None
let ml_input_file : string option ref = ref None
let ml_output_file : string option ref = ref None
let disable_shadow = ref false
let toolchain : string option ref = ref None
let findlib_pkgs : string list ref = ref []
let cma_files : string list ref = ref []

(* not yet configurable, but maybe in the future ... *)
let ocamlfind = match Sys.win32 with
| true -> "ocamlfind.exe"
| false -> "ocamlfind"

type mode =
  | Regular (* extract constants from C *)
  | Emulate (* dummy values, invalid code *)

let mode = ref Regular

let reset () =
  keep_tmp := false;
  c_flags := [];
  ml_input_file := None;
  c_output_file := None;
  ml_output_file := None;
  verbosity := 0;
  ocaml_flags := ["-package";"ctypes"];
  mode := Regular
