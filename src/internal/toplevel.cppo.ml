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
