(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* fdopen: FIXME upstream. The cmi is not installed and the lines
   below are stripped, because they would otherwise depend on
   other interface files that are not installed ... *)

open Ctypes_static

(* See type_printing.mli for the documentation of [format context]. *)
type format_context = [ `toplevel | `array | `nonarray ]

let rec format_typ' : type a. a typ ->
  (format_context -> Format.formatter -> unit) ->
  (format_context -> Format.formatter -> unit) =
  let fprintf = Format.fprintf in
  fun t k context fmt -> match t with
    | Void ->
      fprintf fmt "void%t" (k `nonarray)
    | Primitive _ ->
      let name = Ctypes.string_of_typ t in
      fprintf fmt "%s%t" name (k `nonarray)
    | View { format_typ = Some format ; _ } ->
      format (k `nonarray) fmt
    | View { ty ; _ } ->
      format_typ' ty k context fmt
    | Abstract { aname ; _ } ->
      fprintf fmt "%s%t" aname (k `nonarray)
    | Struct { tag = "" ; fields ; _ } ->
      fprintf fmt "struct {@;<1 2>@[";
      format_fields fields fmt;
      fprintf fmt "@]@;}%t" (k `nonarray)
    | Struct { tag ; spec; fields } ->
      begin match spec, context with
        | Complete _, `toplevel ->
          begin
            fprintf fmt "struct %s {@;<1 2>@[" tag;
            format_fields fields fmt;
            fprintf fmt "@]@;}%t" (k `nonarray)
          end
        | _ -> fprintf fmt "struct %s%t" tag (k `nonarray)
      end
    | Union { utag = ""; ufields ; _ } ->
      fprintf fmt "union {@;<1 2>@[";
      format_fields ufields fmt;
      fprintf fmt "@]@;}%t" (k `nonarray)
    | Union { utag; uspec; ufields } ->
      begin match uspec, context with
        | Some _, `toplevel ->
          begin
            fprintf fmt "union %s {@;<1 2>@[" utag;
            format_fields ufields fmt;
            fprintf fmt "@]@;}%t" (k `nonarray)
          end
        | _ -> fprintf fmt "union %s%t" utag (k `nonarray)
      end
    | Pointer ty ->
      format_typ' ty
        (fun context fmt ->
          match context with
            | `array -> fprintf fmt "(*%t)" (k `nonarray)
            | _      -> fprintf fmt "*%t" (k `nonarray))
        `nonarray fmt
    | Funptr fn ->
      format_fn' fn
        (fun fmt -> Format.fprintf fmt "(*%t)" (k `nonarray)) fmt
    | Array (ty, n) ->
      format_typ' ty (fun _ fmt -> fprintf fmt "%t[%d]" (k `array) n) `nonarray
        fmt
    | Bigarray _ -> ()
    | OCaml String -> format_typ' (ptr char) k context fmt
    | OCaml Bytes -> format_typ' (ptr char) k context fmt
    | OCaml FloatArray -> format_typ' (ptr double) k context fmt

and format_fields : type a. a boxed_field list -> Format.formatter -> unit =
  fun fields fmt ->
  let open Format in
      List.iteri
        (fun _i (BoxedField {ftype=t; fname; _}) ->
          fprintf fmt "@[";
          format_typ' t (fun _ fmt -> fprintf fmt " %s" fname) `nonarray fmt;
          fprintf fmt "@];@;")
        fields
and format_parameter_list parameters k fmt =
  Format.fprintf fmt "%t(@[@[" k;
  if parameters = [] then Format.fprintf fmt "void" else
    List.iteri
      (fun i (BoxedType t) ->
        if i <> 0 then Format.fprintf fmt "@], @[";
        format_typ' t (fun _ _ -> ()) `nonarray fmt)
      parameters;
  Format.fprintf fmt "@]@])"
and format_fn' : 'a. 'a fn ->
  (Format.formatter -> unit) ->
  (Format.formatter -> unit) =
  let rec gather : type a. a fn -> boxed_typ list * boxed_typ =
    function
      | Returns ty -> [], BoxedType ty
      | Function (Void, fn) -> gather fn
      | Function (p, fn) -> let ps, r = gather fn in BoxedType p :: ps, r in
  fun fn k fmt ->
    let ps, BoxedType r = gather fn in
    format_typ' r (fun _context fmt -> format_parameter_list ps k fmt)
      `nonarray fmt

let format_name ?name fmt =
  match name with
    | Some name -> Format.fprintf fmt " %s" name
    | None      -> ()

let format_typ : ?name:string -> Format.formatter -> 'a typ -> unit
  = fun ?name fmt typ ->
    Format.fprintf fmt "@[";
    format_typ' typ (fun _context -> format_name ?name) `nonarray fmt;
    Format.fprintf fmt "@]"

let string_of_typ ?name ty = Format.asprintf "%a" (format_typ ?name) ty
