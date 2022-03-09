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

let toplevel_env = ref Env.empty

let flib_protect f a =
  try f a with Fl_package_base.No_such_package (s, s') ->
    if s' = "" then Printf.eprintf "error: findlib package %s not found\n%!" s
    else Printf.eprintf "error: findlib package %s (%S) not found\n%!" s s' ;
    exit 2

let initialized = ref false

let init ~nopervasives ~pkgs ~use_threads ~cma_files () =
  match !initialized with
  | true -> ()
  | false ->
     if nopervasives then Clflags.nopervasives := true ;
     Toploop.set_paths ();
     toplevel_env := Compmisc.initial_env () ;
     Topfind.log := ignore ;
     let l =
       flib_protect
         (Findlib.package_deep_ancestors ["byte"])
         ["bigarray-compat"; "ctypes"]
     in
     let l = l @ [ "ppx_cstubs" ; "ppx_cstubs.internal" ] in
     CCListLabels.fold_left ~init:[] l ~f:(fun ac el ->
         (flib_protect Findlib.package_directory el)::ac) |>
       CCListLabels.uniq ~eq:CCString.equal |> List.rev |>
       CCListLabels.iter ~f:Topdirs.dir_directory;
     if pkgs <> [] then (
       Topfind.add_predicates ["byte"];
       flib_protect Topfind.don't_load_deeply ["ppx_cstubs.internal"];
       if use_threads then (
         Topfind.add_predicates ["mt";"mt_posix"];
         flib_protect Topfind.load_deeply ["threads"]);
       flib_protect Topfind.load_deeply pkgs );
     ListLabels.iter cma_files ~f:(fun s ->
          let dir = Filename.dirname s in
          if dir <> "." then Topdirs.dir_directory dir ;
          let b = Topdirs.load_file Format.str_formatter s in
          let msg = Format.flush_str_formatter () in
          if not b then (
            Printf.eprintf "fatal:failed to load %s (%s)\n%!" s msg ;
            exit 2 ) ) ;
     initialized := true;
     ()

let eval st =
#if OCAML_VERSION < (4, 12, 0)
    let loc =
      match st with
      | a::_ -> a.Parsetree.pstr_loc
      | [] -> ! Ast_helper.default_loc
    in
#endif
    Typecore.reset_delayed_checks () ;
#if OCAML_VERSION >= (4, 14, 0)
    let (str, _sg, _sn, _shape, newenv) = Typemod.type_structure !toplevel_env st in
#elif OCAML_VERSION >= (4, 12, 0)
    let (str, _sg, _sn, newenv) = Typemod.type_structure !toplevel_env st in
#elif OCAML_VERSION >= (4, 8, 0)
    let (str, _sg, _sn, newenv) = Typemod.type_structure !toplevel_env st loc in
#else
    let str, _sg, newenv = Typemod.type_structure !toplevel_env st loc in
#endif
    let lam = Translmod.transl_toplevel_definition str in
    Warnings.check_fatal () ;
    let init_code, fun_code = Bytegen.compile_phrase lam in
#if OCAML_VERSION >= (4, 8, 0)
    let code, reloc, events =
      Emitcode.to_memory init_code fun_code
    in
#elif OCAML_VERSION >= (4, 3, 0)
    let code, code_size, reloc, events =
      Emitcode.to_memory init_code fun_code
    in
    Meta.add_debug_info code code_size [|events|] ;
#else
    let code,code_size,reloc = Emitcode.to_memory init_code fun_code in
#endif
    let can_free = fun_code = [] in
    let initial_symtable = Symtable.current_state () in
    Symtable.patch_object code reloc ;
    Symtable.check_global_initialized reloc ;
    Symtable.update_global_table () ;
#if OCAML_VERSION >= (4, 8, 0)
    let bytecode, closure = Meta.reify_bytecode code [| events |] None in
    try
      let retval = closure () in
      if can_free then Meta.release_bytecode bytecode;
      toplevel_env := newenv ;
      ignore ( retval : Obj.t );
      ()
    with
    | x ->
      if can_free then Meta.release_bytecode bytecode;
      Symtable.restore_state initial_symtable ;
      raise x
#else
    let free =
      let called = ref false in
      fun () ->
        if can_free && !called = false then (
          called := true ;
#if OCAML_VERSION >= (4, 3, 0)
          Meta.remove_debug_info code;
#endif
          Meta.static_release_bytecode code code_size ;
          Meta.static_free code )
    in
    try
      let res = (Meta.reify_bytecode code code_size) () in
      free () ;
      toplevel_env := newenv ;
      ignore ( res : Obj.t );
      ()
    with x ->
      free () ;
      Symtable.restore_state initial_symtable ;
      raise x
#endif

let get_top () =
  object
    method init ~nopervasives ~pkgs ~use_threads ~cma_files () =
      init ~nopervasives ~pkgs ~use_threads ~cma_files ()
    method eval st = eval st
    method is_merlin_ppx = false
  end

let init () =
  let top = get_top () in
  Ppxc__script._init (Some top)

