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
let debug = ref false
let c_flags : string list ref = ref []
let ocaml_flags : string list ref = ref ocaml_flags_default
let c_output_file : string option ref = ref None
let disable_shadow = ref false
let toolchain : string option ref = ref None
let findlib_pkgs : string list ref = ref []
let cma_files : string list ref = ref []

type mode =
  | Regular (* extract constants from C *)
  | Emulate (* dummy values, invalid code *)

let mode = ref Regular

let reset () =
  debug := false;
  c_flags := [];
  ocaml_flags := ["-package";"ctypes"];
  mode := Regular
