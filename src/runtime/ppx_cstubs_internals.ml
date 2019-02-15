(* Copyright 2018 fdopen

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
   SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
   OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

open Ctypes_static

let rec seal : type a. a Ctypes_static.typ -> size:int -> align:int -> unit =
 fun t ~size ~align ->
  match t with
  | Struct ({spec = Incomplete _; _} as s) ->
    s.fields <- List.rev s.fields ;
    s.spec <- Complete {size; align}
  | Union ({uspec = None; _} as u) ->
    u.ufields <- List.rev u.ufields ;
    u.uspec <- Some {size; align}
  | Struct {tag; _} -> raise (ModifyingSealedType tag)
  | Union {utag; _} -> raise (ModifyingSealedType utag)
  | View {ty; _} -> seal ty ~size ~align
  | _ -> raise (Unsupported "Sealing a non-structured type")

let rec add_field : type t a.
       t Ctypes_static.typ
    -> string
    -> int
    -> a Ctypes_static.typ
    -> (a, t) Ctypes_static.field =
 fun t fname foffset ftype ->
  match t with
  | Struct s ->
    let r = {fname; foffset; ftype} in
    s.fields <- BoxedField r :: s.fields ;
    r
  | Union u ->
    let r = {fname; foffset; ftype} in
    u.ufields <- BoxedField r :: u.ufields ;
    r
  | View {ty; _} ->
    let {ftype; foffset; fname} = add_field ty fname foffset ftype in
    {ftype; foffset; fname}
  | _ -> failwith ("Unexpected field " ^ fname)

external identity : int64 -> int64 = "%identity"

let build_enum : type a b.
       string
    -> a Ctypes.typ
    -> typedef:bool
    -> ?unexpected:(int64 -> b)
    -> (b * a) list
    -> b Ctypes.typ =
 fun name typ ~typedef ?unexpected alist ->
  let fail t =
    Printf.ksprintf failwith "Invalid enum type %s" (Ctypes.string_of_typ t)
  in
  let typedef = if typedef then "" else "enum " in
  let rlist = List.map (fun (l, r) -> (r, l)) alist in
  let unexpected =
    match unexpected with
    | None ->
      let to_string =
        match typ with
        | Ctypes_static.Primitive p ->
          ( match p with
            | Ctypes_primitive_types.Int8_t -> string_of_int
            | Ctypes_primitive_types.Int32_t -> (Int32.to_string : a -> string)
            | Ctypes_primitive_types.Int16_t -> string_of_int
            | Ctypes_primitive_types.Int -> string_of_int
            | Ctypes_primitive_types.Int64_t -> Int64.to_string
            | Ctypes_primitive_types.Uint8_t -> Unsigned.UInt8.to_string
            | Ctypes_primitive_types.Uint16_t -> Unsigned.UInt16.to_string
            | Ctypes_primitive_types.Uint32_t -> Unsigned.UInt32.to_string
            | Ctypes_primitive_types.Uint64_t -> Unsigned.UInt64.to_string
            | _ -> fail typ
            : a -> string )
        | _ -> fail typ
      in
      fun k ->
        Printf.ksprintf failwith "Unexpected enum value for %s: %s" name
          (to_string k)
    | Some f ->
      let to_int64 =
        match typ with
        | Ctypes_static.Primitive p ->
          ( match p with
            | Ctypes_primitive_types.Int8_t -> Int64.of_int
            | Ctypes_primitive_types.Int32_t -> Int64.of_int32
            | Ctypes_primitive_types.Int16_t -> Int64.of_int
            | Ctypes_primitive_types.Int -> Int64.of_int
            | Ctypes_primitive_types.Int64_t -> identity
            | Ctypes_primitive_types.Uint8_t -> Unsigned.UInt8.to_int64
            | Ctypes_primitive_types.Uint16_t -> Unsigned.UInt16.to_int64
            | Ctypes_primitive_types.Uint32_t -> Unsigned.UInt32.to_int64
            | Ctypes_primitive_types.Uint64_t -> Unsigned.UInt64.to_int64
            | _ -> fail typ
            : a -> int64 )
        | _ -> fail typ
      in
      fun k -> f (to_int64 k)
  in
  let write k = List.assoc k alist
  and read k = try List.assoc k rlist with Not_found -> unexpected k
  and format_typ k fmt = Format.fprintf fmt "%s%s%t" typedef name k in
  Ctypes_static.view ~format_typ ~read ~write typ

let build_enum_bitmask : type a b.
    string -> a Ctypes.typ -> typedef:bool -> (b * a) list -> b list Ctypes.typ
    =
 fun name typ ~typedef alist ->
  let fail t =
    Printf.ksprintf failwith "Invalid enum type %s" (Ctypes.string_of_typ t)
  in
  let lor', land', zero =
    match typ with
    | Ctypes_static.Primitive p ->
      ( match p with
        | Ctypes_primitive_types.Int8_t -> (( lor ), ( land ), 0)
        | Ctypes_primitive_types.Int16_t -> (( lor ), ( land ), 0)
        | Ctypes_primitive_types.Int -> (( lor ), ( land ), 0)
        | Ctypes_primitive_types.Int32_t -> Int32.(logor, logand, zero)
        | Ctypes_primitive_types.Int64_t -> Int64.(logor, logand, zero)
        | Ctypes_primitive_types.Uint8_t ->
          Unsigned.UInt8.(logor, logand, zero)
        | Ctypes_primitive_types.Uint16_t ->
          Unsigned.UInt16.(logor, logand, zero)
        | Ctypes_primitive_types.Uint32_t ->
          Unsigned.UInt32.(logor, logand, zero)
        | Ctypes_primitive_types.Uint64_t ->
          Unsigned.UInt64.(logor, logand, zero)
        | _ -> fail typ
        : (a -> a -> a) * (a -> a -> a) * a )
    | _ -> fail typ
  in
  let typedef = if typedef then "" else "enum " in
  let (write : b list -> a) =
   fun l -> List.fold_left (fun ac k -> lor' (List.assoc k alist) ac) zero l
  and (read : a -> b list) =
   fun res ->
    List.fold_left
      (fun ac (a, b) -> if land' b res <> zero then a :: ac else ac)
      [] alist
  (* FIXME: remaining set bits ? *)
  and format_typ k fmt = Format.fprintf fmt "%s%s%t" typedef name k in
  Ctypes_static.view ~format_typ ~read ~write typ

external to_voidp : nativeint -> Cstubs_internals.voidp = "%identity"

let invalid_code () = failwith "ppx_cstub generated invalid code"

(* just to reduce the lines of biolerplate code *)
let string_write : string -> char Ctypes_static.ptr =
  match (Ctypes.string : string Ctypes.typ) with
  | Ctypes_static.View
      { Ctypes_static.write = f
      ; Ctypes_static.ty =
          Ctypes_static.Pointer
            (Ctypes_static.Primitive Ctypes_primitive_types.Char)
      ; _ } ->
    f
  | _ -> assert false

let string_read : nativeint -> string =
  match (Ctypes.string : string Ctypes.typ) with
  | Ctypes_static.View
      { Ctypes_static.read = f
      ; Ctypes_static.ty =
          Ctypes_static.Pointer
            (Ctypes_static.Primitive Ctypes_primitive_types.Char)
      ; _ } ->
    fun x -> f (Cstubs_internals.make_ptr Ctypes.char (to_voidp x))
  | _ -> assert false

let string_opt_read x = if x = 0n then None else Some (string_read x)

let string_opt_write : string option -> char Ctypes_static.ptr =
  match (Ctypes.string_opt : string option Ctypes.typ) with
  | Ctypes_static.View
      { Ctypes_static.write = f
      ; Ctypes_static.ty =
          Ctypes_static.Pointer
            (Ctypes_static.Primitive Ctypes_primitive_types.Char)
      ; _ } ->
    f
  | _ -> assert false
