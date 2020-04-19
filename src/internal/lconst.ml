(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018-2020 fdopen
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

let type_mod_name = ref ""

let impl_mod_name = ref ""

let type_modtype_name = ref ""

let clear () =
  impl_mod_name := "Ppxc_private_impl";
  type_mod_name := "Ppxc_private_types";
  type_modtype_name := "__ppxc_private"

let () = clear ()

let init usf =
  let use_open_struct = Ocaml_config.use_open_struct () in
  (type_modtype_name :=
     match usf with "" -> "__ppxc_private_types" | s -> "__ppxc_" ^ s);
  let name = match usf with "" -> "Ppxc_private" | s -> "Ppxc_" ^ s in
  if use_open_struct then (
    (impl_mod_name := match usf with "" -> "Ppxc__private" | s -> "Ppxc__" ^ s);
    type_mod_name := name )
  else (
    impl_mod_name := name;
    type_mod_name :=
      match usf with
      | "" -> "Ppxc_private_types"
      | s -> String.concat "_" [ "Ppxc"; s; "types" ] )
