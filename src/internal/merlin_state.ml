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

type to_child = {
  opt : Options.merlin_state;
  sr : Script_result.merlin_state;
  ur : Uniq_ref.merlin_state;
  lc : Lconst.merlin_state;
  ui : Uniq_ids.merlin_state;
}

type from_child = {
  fsr : Script_result.merlin_state;
  fur : Uniq_ref.merlin_state;
  fui : Uniq_ids.merlin_state;
}

let to_child () =
  {
    opt = Options.merlin_save ();
    sr = Script_result.merlin_save ();
    ur = Uniq_ref.merlin_save ();
    lc = Lconst.merlin_save ();
    ui = Uniq_ids.merlin_save ();
  }

let from_parent { opt; sr; ur; lc; ui } =
  Options.merlin_restore opt;
  Script_result.merlin_restore sr;
  Uniq_ref.merlin_restore ur;
  Lconst.merlin_restore lc;
  Uniq_ids.merlin_restore ui

let to_parent () =
  {
    fsr = Script_result.merlin_save ();
    fur = Uniq_ref.merlin_save ();
    fui = Uniq_ids.merlin_save ();
  }

let from_child { fsr; fur; fui } =
  Script_result.merlin_restore fsr;
  Uniq_ref.merlin_restore fur;
  Uniq_ids.merlin_restore fui

type error =
  (* fixme: more errors? *)
  | Location of Location.t * string
  | Env of Env.error
  | Typecore of Location.t * Env.t * Typecore.error
  | Typedecl of Location.t * Typedecl.error

let to_error x =
  let er x =
    let a, b = Toplevel.serialize_location_error x in
    Location (a, b)
  in
  let common x =
    let s = Printexc.to_string x in
    try Std.Util.error "%s" s with Location.Error x -> er x
  in
  let catch e =
    match ignore (Marshal.to_string e [] : string) with
    | () -> e
    | exception Invalid_argument _ -> common x
  in
  match x with
  | Location.Error x -> er x
  | Env.Error e -> catch (Env e)
  | Typecore.Error (e, f, g) -> catch (Typecore (e, f, g))
  | Typedecl.Error (l, e) -> catch (Typedecl (l, e))
  | x -> common x

let raise_error = function
  | Location (loc, s) -> Std.Util.error ~loc "%s" s
  | Env x -> raise (Env.Error x)
  | Typecore (a, b, c) -> raise (Typecore.Error (a, b, c))
  | Typedecl (a, b) -> raise (Typedecl.Error (a, b))
