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

type 'a return = { return: 'b. 'a -> 'b }
let with_return (type a) f =
  let module E = struct exception E of a end in
  try
    f {return = fun x -> raise_notrace (E.E x)}
  with
    E.E r -> r

let finally ~h f = CCFun.finally ~h ~f

external identity : 'a -> 'a = "%identity"

module Util = struct
  open Migrate_parsetree.Ast_405

  let with_loc loc f =
    let old_loc = !Ast_helper.default_loc in
    Ast_helper.default_loc := loc;
    finally ~h:(fun () -> Ast_helper.default_loc := old_loc) f

  let error ?(loc= !Ast_helper.default_loc) fmt =
    Format.ksprintf (fun s ->
      raise (Location.Error (Location.error ~loc s))) fmt

  let str_expr ?loc s = Ast_helper.(Exp.constant ?loc (Const.string s))
  let int_expr ?loc ?attrs i =
    Ast_helper.(Exp.constant ?loc ?attrs (Const.int i))

  let mk_loc s =
    Location.mkloc s !Ast_helper.default_loc

  let mk_lid ?(loc= !Ast_helper.default_loc) s =
    match CCString.split_on_char '.' s with
    | [] -> assert false
    | hd::tl ->
      let rec iter accu = function
      | [] -> accu
      | hd::tl -> iter (Longident.Ldot(accu,hd)) tl in
      let res = iter (Longident.Lident hd) tl in
      Location.mkloc res loc

  let mk_pat s = Ast_helper.Pat.var (mk_loc s)

  let safe_ascii c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ||
    (c >= '0' && c <= '9')

  let safe_ascii_only s =
    CCString.filter_map (fun c -> if safe_ascii c then Some c else None) s

  let safe_ascii_only_ml s =
    CCString.filter_map (fun c ->
      if safe_ascii c || c = '\'' then Some c else None) s

  let safe_cname =
    let i = ref (-1) in
    fun ?prefix () ->
      let module Lo = Location in
      let module Le = Lexing in
      let loc = !Ast_helper.default_loc in
      let name = Filename.basename loc.Lo.loc_start.Le.pos_fname in
      let name = match CCString.Split.left ~by:"." name with
      | None -> ""
      | Some(s,_) -> safe_ascii_only s in
      let s,p = match prefix with
      | None -> "",""
      | Some s -> safe_ascii_only s,"_" in
      let cutmax s maxlen =
        let len = String.length s in
        if len > maxlen then
          String.sub s 0 maxlen
        else
          s in
      (* TODO: there seems to be a limit for msvc *)
      let s = cutmax s 20 in
      let name = cutmax name 40 in
      incr i;
      Printf.sprintf "ppxc%x_%s_%x_%x%s%s"
        !i name
        loc.Lo.loc_start.Le.pos_lnum
        loc.Lo.loc_start.Le.pos_cnum
        p s

  let safe_mlname =
    let i = ref (-1) in
    fun ?prefix () ->
      let s,p = match prefix with
      | None -> "",""
      | Some s -> safe_ascii_only_ml s,"_" in
      let loc = !Ast_helper.default_loc in
      let line = loc.Location.loc_start.Lexing.pos_lnum in
      incr i;
      Printf.sprintf
        "ppxc__%s%sline%d_%d" s p line !i
end
