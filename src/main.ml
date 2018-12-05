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

let ml_output = ref ""
let c_output = ref ""
let pretty_print = ref false
let cflags = ref []
let oflags = ref (List.rev Options.ocaml_flags_default)
let keep_tmp = ref false
let disable_shadow = ref false
let toolchain = ref None
let findlib_pkgs = ref []
let cma_files = ref []

let spec = Arg.align
    [ "-o-ml", Arg.Set_string ml_output,
      "<file>    write generated OCaml file to <file>";
      "-o-c", Arg.Set_string c_output,
      "<file>    write generated C file to <file>";
      "-cflag", Arg.String (fun s -> cflags := s :: !cflags),
      "<opt>     Pass option <opt> to the C compiler";
      "-oflag", Arg.String (fun s -> oflags := s :: !oflags),
      "<opt>     Pass option <opt> to ocamlfind";
      "-pkg", Arg.String (fun s -> findlib_pkgs := s :: !findlib_pkgs),
      "<opt>     import types from findlib package <opt>";
      "-toolchain", Arg.String (fun s -> toolchain := Some s),
      "<opt>     use ocamlfind toolchain <opt>";
      "-keep-tmp", Arg.Set keep_tmp,
      "     Don't delete temporary files";
      "-disable-shadowing", Arg.Set disable_shadow,
      "     Don't shadow the Ctypes module to hide unsafe functions";
      "-pretty", Arg.Set pretty_print,
      "     Print a human readable ml file instead of the binary ast" ]

let executable = Filename.basename Sys.executable_name
let usage =
  Printf.sprintf
    "%s [<file>] -o-ml my_module.ml -o-c my_modul_stubs.c"
    executable

let error_exit s =
  Printf.eprintf
    "%s: %s Try %s --help\n"
    executable  s executable;
  exit 1

let run_main () =
  set_binary_mode_out stdout true;
  set_binary_mode_out stderr true;
  set_binary_mode_in stdin true;
  Ppx_cstubs.init ();
  Migrate_parsetree.Driver.run_main ()

let main () =
  let source = ref None in
  Arg.parse spec (fun a ->
    if a = "" || a.[0] = '-' then
      raise (Arg.Bad a);
    if Filename.check_suffix a ".cma" ||
       Filename.check_suffix a ".cmo" then
      cma_files := a :: !cma_files
    else (
      if !source <> None then
        raise (Arg.Bad a);
      source := Some a)) usage;
  let source = match !source with
  | None -> error_exit "no source file specified"
  | Some s -> s in
  let ml_output = match !ml_output with
  | "" -> error_exit "ml output file missing"
  | c -> c in
  let c_output = match !c_output with
  | "" -> error_exit "name of c stub file missing"
  | s -> s in
  if ml_output = c_output then
    error_exit "use different output files";

  let h s =
    let s = Filename.basename s in
    try
      Filename.chop_extension s
    with
    | Invalid_argument _ -> s in

  if h ml_output = h c_output then
    error_exit "filenames must differ, not only their suffix";

  Options.ocaml_flags := List.rev !oflags;
  Options.c_flags := List.rev !cflags;
  Options.debug := !keep_tmp;
  Options.mode := Options.Regular;
  Options.c_output_file := Some c_output;
  Options.disable_shadow := !disable_shadow;
  Options.toolchain := !toolchain;
  Options.cma_files := List.rev !cma_files;
  Options.findlib_pkgs := List.rev !findlib_pkgs;
  if !findlib_pkgs <> [] || !cma_files <> [] then
    Toplevel.init (); (* trigger exceptions *)

  Ocaml_config.init ();

  let l = if ml_output = "-" then [] else "-o" :: ml_output :: [] in
  let l = source :: l in
  let l = if !pretty_print then l else "--dump-ast"::l in
  let l = Sys.argv.(0) :: l in
  (* what an evil hack .... *)
  let new_argv = Array.of_list l in
  let orig_argv_length = Array.length Sys.argv in
  let new_argv_length = Array.length new_argv in
  assert (new_argv_length <= orig_argv_length);
  ArrayLabels.blit ~src:new_argv ~src_pos:0
    ~dst:Sys.argv ~dst_pos:0 ~len:new_argv_length;
  if new_argv_length <> orig_argv_length then
    Obj.truncate (Obj.repr Sys.argv) new_argv_length;
  Arg.current := 0;
  run_main ()

let dummy () =
  Options.mode := Options.Emulate;
  run_main ()

let () =
  let fln = Filename.basename Sys.executable_name |> CCString.lowercase_ascii in
  if Sys.argv.(1) = "--as-ppx" && CCString.mem ~sub:"merlin" fln then
    dummy ()
  else
    main ()
