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

let toplevel_env = ref Env.empty

let findlib_protect f a =
  try
    f a
  with
  | Fl_package_base.No_such_package(s,s') ->
    if s' = "" then
      Printf.eprintf "error: findlib package %s not found\n%!" s
    else
      Printf.eprintf "error: findlib package %s (%S) not found\n%!" s s';
    exit 2

let init = lazy (
  if !Options.nopervasives then
    Clflags.nopervasives := true;
  toplevel_env := Compmisc.initial_env ();
  Topfind.log := ignore;
  Topdirs.dir_directory @@ Findlib.package_directory "integers";
  Topdirs.dir_directory @@ Findlib.package_directory "ctypes";
  let (//) = Filename.concat in
  let add_cmi_dir_findlib =
    (* Interface folder differs, if it is not yet installed (tests,...) *)
    let b = Filename.basename Sys.executable_name in
    if b <> "ppx_cstubs.run" then true else
    let dir = Filename.dirname Sys.executable_name in
    let fln1 = dir // "OMakefile" in
    let fln2 = dir // "toplevel.ml" in
    not (Sys.file_exists fln1 &&
       Sys.file_exists fln2) in
  if add_cmi_dir_findlib then
    let dir = Findlib.package_directory "ppx_cstubs" // "internal" in
    Topdirs.dir_directory dir
  else
    Topdirs.dir_directory (Filename.dirname Sys.executable_name);

  if !Options.findlib_pkgs <> [] then (
    let l = [
      "compiler-libs.bytecomp";
      "compiler-libs.toplevel";
      "containers";
      "ctypes";
      "findlib.top";
      "integers";
      "num";
      "ocaml-migrate-parsetree";
      "ppx_tools_versioned";
      "re.perl";
    ] in
    let l =
      if Ocaml_config.runtime_version >= (4,3,0) then l
      else "result"::l in
    let l = if add_cmi_dir_findlib then "ppx_cstubs"::l else l in
    Topfind.predicates := ["byte"];
    findlib_protect Topfind.don't_load_deeply l;
    findlib_protect Topfind.load_deeply !Options.findlib_pkgs
  );

  ListLabels.iter !Options.cma_files ~f:(fun s ->
    let dir = Filename.dirname s in
    if dir <> "." then
      Topdirs.dir_directory dir;
    let b = Topdirs.load_file Format.str_formatter s in
    let msg = Format.flush_str_formatter () in
    if not b then (
      Printf.eprintf "fatal:failed to load %s (%s)\n%!" s msg;
      exit 2 ));
)

let init () = Lazy.force init

let eval_expression =
  let module M = Migrate_parsetree in
  let module A = Mparsetree.Ast_cur in
  let module P = A.Parsetree in
  let to_current = M.Versions.(migrate Mparsetree.ast_version ocaml_current) in
  (* see OCaml's toplevel/toploop.ml for details *)
  fun expr ->
    let loc = !A.Ast_helper.default_loc in
    let str = [{
        P.pstr_desc = P.Pstr_eval(expr,[]);
        P.pstr_loc = loc;
      }] in
    let st = to_current.M.Versions.copy_structure str in
    Typecore.reset_delayed_checks ();
    let str, _sg, newenv = Typemod.type_structure !toplevel_env st loc in
    let lam = Translmod.transl_toplevel_definition str in
    Warnings.check_fatal ();
    let init_code, fun_code = Bytegen.compile_phrase lam in
#if OCAML_VERSION >= (4, 3, 0)
    let code,code_size,reloc,events = Emitcode.to_memory init_code fun_code in
    Meta.add_debug_info code code_size [| events |];
#else
    let code,code_size,reloc = Emitcode.to_memory init_code fun_code in
#endif
    let can_free = (fun_code = []) in
    let initial_symtable = Symtable.current_state () in
    Symtable.patch_object code reloc;
    Symtable.check_global_initialized reloc;
    Symtable.update_global_table();
    let free =
      let called = ref false in
      fun () ->
        if can_free && !called = false then (
          called := true;
#if OCAML_VERSION >= (4, 3, 0)
          Meta.remove_debug_info code;
#endif
          Meta.static_release_bytecode code code_size;
          Meta.static_free code) in
    try
      let res = (Meta.reify_bytecode code code_size) () in
      free ();
      toplevel_env := newenv;
      res
    with x ->
      free ();
      Symtable.restore_state initial_symtable;
      raise x
