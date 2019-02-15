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

let executable = Filename.basename Sys.executable_name

let error_exit s =
  Printf.eprintf "%s: %s Try %s --help\n" executable s executable ;
  exit 1

let common_main () =
  set_binary_mode_out stdout true ;
  set_binary_mode_out stderr true ;
  set_binary_mode_in stdin true ;
  Ppx_cstubs.init () ;
  Migrate_parsetree.Driver.run_main ()

let cpp_main () =
  let usage =
    Printf.sprintf "%s [<file>] -o-ml my_module.ml -o-c my_module_stubs.c"
      executable
  in
  let anon_is_target = ref false in
  let na f p =
    anon_is_target := false ;
    f p
  in
  let arg_set_string v = Arg.String (na (fun s -> v := s)) in
  let arg_set v = Arg.Unit (na (fun () -> v := true)) in
  let arg_string f = Arg.String (na f) in
  let arg_set_int v = Arg.Int (na (fun a -> v := a)) in
  let ml_output = ref "" in
  let c_output = ref "" in
  let pretty_print = ref false in
  let cflags = ref [] in
  let cflags_rest = ref [] in
  let oflags = ref (List.rev Options.ocaml_flags_default) in
  let include_dirs = ref [] in
  let keep_tmp = ref false in
  let toolchain = ref None in
  let findlib_pkgs = ref [] in
  let cma_files = ref [] in
  let verbose = ref 0 in
  let absname = ref false in
  let nopervasives = ref false in
  let set_output_by_suffix s =
    match CCString.Split.right ~by:"." s with
    | None ->
      if CCString.lowercase_ascii s = "none" then c_output := s
      else raise (Arg.Bad s)
    | Some (_, suf) -> (
      if String.length suf = 0 then raise (Arg.Bad s) ;
      match CCChar.lowercase_ascii suf.[0] with
      | 'r' | 'm' -> ml_output := s
      | 'c' -> c_output := s
      | _ -> raise (Arg.Bad s) )
  in
  let spec =
    Arg.align
      [ ( "-o-ml"
        , arg_set_string ml_output
        , "<file>    write generated OCaml file to <file>" )
      ; ( "-o-c"
        , arg_set_string c_output
        , "<file>    write generated C file to <file>. 'none' to disable c \
           output" )
      ; ( "-o"
        , Arg.String
            (fun s ->
              anon_is_target := true ;
              set_output_by_suffix s )
        , "<file1>\xC2\xA0<file2>    \xC2\xA0write generated files to <file1> \
           and <file2>. The files must have proper suffixes." )
      ; ( "-cflag"
        , arg_string (fun s -> cflags := s :: !cflags)
        , "<opt>     Pass option <opt> to the C compiler" )
      ; ( "-I"
        , arg_string (fun s ->
              include_dirs := s :: !include_dirs ;
              oflags := s :: "-I" :: !oflags ;
              anon_is_target := false )
        , "<dir>     Add <dir> to the list of include directories" )
      ; ( "-pkg"
        , arg_string (fun s -> findlib_pkgs := s :: !findlib_pkgs)
        , "<opt>     import types from findlib package <opt>" )
      ; ( "-toolchain"
        , arg_string (fun s -> toolchain := Some s)
        , "<opt>     use ocamlfind toolchain <opt>" )
      ; ("-keep-tmp", arg_set keep_tmp, "     Don't delete temporary files")
      ; ( "-pretty"
        , arg_set pretty_print
        , "     Print a human readable ml file instead of the binary ast" )
      ; ( "-verbose"
        , arg_set_int verbose
        , "<level>   Set the level of verbosity. By default, it is set to 0, \
           what means no debug messages at all on stderr" )
      ; ( "-absname"
        , arg_set absname
        , "     Show absolute filenames in error messages" )
      ; ("-nopervasives", arg_set nopervasives, "     (undocumented)")
      ; ( "--"
        , Arg.Rest (fun a -> cflags_rest := a :: !cflags_rest)
        , "     Pass all following parameters verbatim to the c compiler" ) ]
  in
  let add_cma_file a =
    if Sys.file_exists a then cma_files := a :: !cma_files
    else
      let c =
        ListLabels.exists !include_dirs ~f:(fun d ->
            let a = Filename.concat d a in
            if Sys.file_exists a then (
              cma_files := a :: !cma_files ;
              true )
            else false )
      in
      if c = false then Printf.sprintf "%S doesn't exist\n" a |> error_exit
  in
  let source = ref None in
  Arg.parse spec
    (fun a ->
      if a = "" then raise (Arg.Bad a) ;
      if !anon_is_target then (
        anon_is_target := false ;
        set_output_by_suffix a )
      else
        let la = CCString.lowercase_ascii a in
        if Filename.check_suffix la ".cma" || Filename.check_suffix la ".cmo"
        then add_cma_file a
        else (
          if !source <> None then raise (Arg.Bad a) ;
          source := Some a ) )
    usage ;
  let source =
    match !source with
    | None -> error_exit "no source file specified"
    | Some s -> s
  in
  let ml_output =
    match !ml_output with "" -> error_exit "ml output file missing" | c -> c
  in
  let c_output =
    match !c_output with
    | "" -> error_exit "c stub file not specified"
    | s -> s
  in
  if ml_output = c_output then error_exit "use different output files" ;
  if !absname then Location.absname := true ;
  let h s =
    let s = CCString.lowercase_ascii s in
    let s = Filename.basename s in
    try Filename.chop_extension s with Invalid_argument _ -> s
  in
  if CCString.lowercase_ascii c_output <> "none" then (
    if h ml_output = h c_output then
      error_exit "filenames must differ, not only their suffix" ;
    Options.c_output_file := Some c_output ) ;
  Options.ocaml_flags := List.rev !oflags ;
  Options.c_flags := List.rev !cflags @ List.rev !cflags_rest ;
  Options.keep_tmp := !keep_tmp ;
  Options.nopervasives := !nopervasives ;
  Options.mode := Options.Regular ;
  Options.ml_input_file := Some source ;
  Options.ml_output_file := Some ml_output ;
  Options.toolchain := !toolchain ;
  Options.cma_files := List.rev !cma_files ;
  Options.findlib_pkgs := List.rev !findlib_pkgs ;
  Options.verbosity := !verbose ;
  if !findlib_pkgs <> [] || !cma_files <> [] then Toplevel.init () ;
  (* trigger exceptions *)
  Ocaml_config.init () ;
  let l = if ml_output = "-" then [] else ["-o"; ml_output] in
  let l = source :: l in
  let l = if !pretty_print then l else "--dump-ast" :: l in
  let l = Sys.argv.(0) :: l in
  (* what an evil hack .... *)
  let new_argv = Array.of_list l in
  let orig_argv_length = Array.length Sys.argv in
  let new_argv_length = Array.length new_argv in
  assert (new_argv_length <= orig_argv_length) ;
  ArrayLabels.blit ~src:new_argv ~src_pos:0 ~dst:Sys.argv ~dst_pos:0
    ~len:new_argv_length ;
  if new_argv_length <> orig_argv_length then
    Obj.truncate (Obj.repr Sys.argv) new_argv_length ;
  Arg.current := 0 ;
  common_main ()

let merlin_main () =
  Options.mode := Options.Emulate ;
  common_main ()

let init () =
  if CCArray.exists (( = ) "--as-ppx") Sys.argv then merlin_main ()
  else cpp_main ()
