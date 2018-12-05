(* Copyright 2018 fdopen

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
   CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
   OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
   NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

open Ctypes_static

let rec seal : type a. a Ctypes_static.typ -> size:int -> align:int -> unit =
  fun t ~size ~align ->
    match t with
    | Struct ({ spec = Incomplete _ ; _ } as s) ->
      s.spec <- Complete { size ; align }
    | Union ({ uspec = None ; _ } as u ) ->
      u.uspec <- Some { size; align }
    | Struct { tag; _ } ->
      raise (ModifyingSealedType tag)
    | Union { utag; _ } ->
      raise (ModifyingSealedType utag)
    | View { ty ; _ } -> seal ty ~size ~align
    | _ -> raise (Unsupported "Sealing a non-structured type")

let rec add_field :
  type t a. t Ctypes_static.typ -> string -> int ->
  a Ctypes_static.typ -> (a, t) Ctypes_static.field =
  fun t fname foffset ftype ->
    match t with
    | Struct (s) ->
      let r = {fname; foffset; ftype} in
      s.fields <- BoxedField r :: s.fields;
      r
    | Union (u) ->
      let r = {fname; foffset; ftype} in
      u.ufields <- BoxedField r :: u.ufields;
      r
    | View { ty ; _ } ->
      let { ftype; foffset; fname } = add_field ty fname foffset ftype in
      { ftype; foffset; fname }
    | _ -> failwith ("Unexpected field "^ fname)

external to_voidp : nativeint -> Cstubs_internals.voidp = "%identity"
let invalid_code () = failwith "ppx_cstub generated invalid code"
