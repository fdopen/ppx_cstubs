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

open Mparsetree.Ast_cur
module Lo = Location
module Le = Lexing

let safe_ascii c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || c = '_'
  || (c >= '0' && c <= '9')

let safe_ascii_only s =
  CCString.filter_map (fun c -> if safe_ascii c then Some c else None) s

let safe_ascii_only_ml s =
  CCString.filter_map
    (fun c -> if safe_ascii c || c = '\'' then Some c else None)
    s

let unsuffixed_file_name () =
  let loc = !Ast_helper.default_loc in
  let name = Filename.basename loc.Lo.loc_start.Le.pos_fname in
  match CCString.split_on_char '.' name with
  | [] -> ""
  | s :: _ -> safe_ascii_only s

let make_uniq_cnt htl s =
  let i = match Hashtbl.find htl s with exception Not_found -> 0 | n -> n in
  Hashtbl.replace htl s (succ i);
  i

let htl_c = Hashtbl.create 128

let safe_cname =
  let cnt = make_uniq_cnt htl_c in
  fun ~prefix ->
    let loc = !Ast_helper.default_loc in
    let name = unsuffixed_file_name () in
    let s = safe_ascii_only prefix in
    let cutmax s maxlen =
      let len = String.length s in
      if len > maxlen then String.sub s 0 maxlen else s
    in
    (* TODO: there seems to be a limit for msvc *)
    let s = cutmax s 20 in
    let name = cutmax name 40 in
    let line = loc.Lo.loc_start.Le.pos_lnum in
    let cnum = loc.Lo.loc_start.Le.pos_cnum in
    let res = Printf.sprintf "%s_%x_%x_%s" name line cnum s in
    match cnt res with
    | 0 -> "ppxc_" ^ res
    | i -> Printf.sprintf "ppxc%x_%s" i res

let htl_ml = Hashtbl.create 128

let safe_mlname =
  let cnt = make_uniq_cnt htl_ml in
  fun ?(capitalize = false) ?prefix () ->
    let s, p =
      match prefix with
      | None -> ("", "")
      | Some s -> (safe_ascii_only_ml s, "_")
    in
    let loc = !Ast_helper.default_loc in
    let line = loc.Lo.loc_start.Le.pos_lnum in
    let pre =
      if capitalize then Myconst.private_prefix_capitalized
      else Myconst.private_prefix
    in
    let f = pre.[0] in
    let pre = String.sub pre 1 (String.length pre - 1) in
    let res = Printf.sprintf "%c%s%s%sline%d" f pre s p line in
    match cnt res with 0 -> res | i -> Printf.sprintf "%s_%d" res i

type merlin_state = {
  l_c : (string * int) list;
  l_ml : (string * int) list;
}

let merlin_save () : merlin_state =
  { l_c = CCHashtbl.Poly.to_list htl_c; l_ml = CCHashtbl.Poly.to_list htl_ml }

let merlin_restore { l_c; l_ml } =
  let f htl l =
    Hashtbl.clear htl;
    List.iter (fun (a, b) -> Hashtbl.replace htl a b) l
  in
  f htl_c l_c;
  f htl_ml l_ml
