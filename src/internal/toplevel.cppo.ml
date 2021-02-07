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

let init top =
  let module O = Options in
  top#init
    ~nopervasives:!O.nopervasives
    ~pkgs:!O.findlib_pkgs
    ~use_threads:(Std.Various.use_threads ())
    ~cma_files:!O.cma_files ()

let set_absname x =
#if OCAML_VERSION >= (4, 8, 0)
  Clflags.absname := x
#else
  Location.absname := x
#endif

let serialize_location_error x =
#if OCAML_VERSION >= (4, 8, 0)
  let main = x.Location.main in
  let b = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "@[%t@]" main.Location.txt;
  Format.pp_print_flush fmt ();
  (main.Location.loc, Buffer.contents b)
#else
  x.Location.loc, x.Location.msg
#endif

#if OCAML_VERSION >= (4, 9, 0)
external caml_sys_modify_argv : string array -> unit = "caml_sys_modify_argv"
#endif

let set_argv new_argv =
#if OCAML_VERSION >= (4, 9, 0)
  caml_sys_modify_argv new_argv ;
#else
  let orig_argv_length = Array.length Sys.argv in
  let new_argv_length = Array.length new_argv in
  assert (new_argv_length <= orig_argv_length) ;
  ArrayLabels.blit ~src:new_argv ~src_pos:0 ~dst:Sys.argv ~dst_pos:0
    ~len:new_argv_length ;
  if new_argv_length <> orig_argv_length then
    Obj.truncate (Obj.repr Sys.argv) new_argv_length ;
#endif
  Arg.current := 0
