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
  | Struct ({ spec = Incomplete _; _ } as s) ->
    s.fields <- List.rev s.fields;
    s.spec <- Complete { size; align }
  | Union ({ uspec = None; _ } as u) ->
    u.ufields <- List.rev u.ufields;
    u.uspec <- Some { size; align }
  | Struct { tag; _ } -> raise (ModifyingSealedType tag)
  | Union { utag; _ } -> raise (ModifyingSealedType utag)
  | View { ty; _ } -> seal ty ~size ~align
  | _ -> raise (Unsupported "Sealing a non-structured type")

let rec add_field :
    type t a.
    t Ctypes_static.typ ->
    string ->
    int ->
    a Ctypes_static.typ ->
    (a, t) Ctypes_static.field =
 fun t fname foffset ftype ->
  match t with
  | Struct s ->
    let r = { fname; foffset; ftype } in
    s.fields <- BoxedField r :: s.fields;
    r
  | Union u ->
    let r = { fname; foffset; ftype } in
    u.ufields <- BoxedField r :: u.ufields;
    r
  | View { ty; _ } ->
    let ({ fname = _; _ } as r) = add_field ty fname foffset ftype in
    r
  | _ -> failwith ("Unexpected field " ^ fname)

external identity : 'a -> 'a = "%identity"

let build_enum :
    type a b.
    string ->
    a Ctypes.typ ->
    typedef:bool ->
    ?unexpected:(int64 -> b) ->
    (b * a) list ->
    b Ctypes.typ =
 fun name typ ~typedef ?unexpected alist ->
  let fail t =
    Printf.ksprintf failwith "Invalid enum type %s" (Ctypes.string_of_typ t)
  in
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
  let pname = if typedef then name else "enum " ^ name in
  let write k = List.assoc k alist
  and read k = try List.assoc k rlist with Not_found -> unexpected k
  and format_typ k fmt = Format.fprintf fmt "%s%t" pname k in
  Ctypes_static.view ~format_typ ~read ~write typ

let build_enum_bitmask :
    type a b.
    string ->
    a Ctypes.typ ->
    typedef:bool ->
    ?unexpected:(b list -> int64 -> b list) ->
    (b * a) list ->
    b list Ctypes.typ =
 fun name typ ~typedef ?unexpected alist ->
  let fail t =
    Printf.ksprintf failwith "Invalid enum type %s" (Ctypes.string_of_typ t)
  in
  let lor', land', zero, lnot' =
    match typ with
    | Ctypes_static.Primitive p ->
      ( match p with
        | Ctypes_primitive_types.Int8_t -> (( lor ), ( land ), 0, lnot)
        | Ctypes_primitive_types.Int16_t -> (( lor ), ( land ), 0, lnot)
        | Ctypes_primitive_types.Int -> (( lor ), ( land ), 0, lnot)
        | Ctypes_primitive_types.Int32_t -> Int32.(logor, logand, zero, lognot)
        | Ctypes_primitive_types.Int64_t -> Int64.(logor, logand, zero, lognot)
        | Ctypes_primitive_types.Uint8_t ->
          Unsigned.UInt8.(logor, logand, zero, lognot)
        | Ctypes_primitive_types.Uint16_t ->
          Unsigned.UInt16.(logor, logand, zero, lognot)
        | Ctypes_primitive_types.Uint32_t ->
          Unsigned.UInt32.(logor, logand, zero, lognot)
        | Ctypes_primitive_types.Uint64_t ->
          Unsigned.UInt64.(logor, logand, zero, lognot)
        | _ -> fail typ
        : (a -> a -> a) * (a -> a -> a) * a * (a -> a) )
    | _ -> fail typ
  in
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
      fun _ k ->
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
      fun a k -> f a (to_int64 k)
  in
  let pname = if typedef then name else "enum " ^ name in
  let ralist = List.rev alist in
  let (write : b list -> a) =
   fun l -> List.fold_left (fun ac k -> lor' (List.assoc k alist) ac) zero l
  and (read : a -> b list) =
   fun res ->
    let rec iter res_orig ac res l =
      match l with
      | [] -> if res = zero then ac else unexpected ac res
      | (a, b) :: tl ->
        if land' b res_orig = b then
          iter res_orig (a :: ac) (land' res (lnot' b)) tl
        else iter res_orig ac res tl
    in
    iter res [] res ralist
  and format_typ k fmt = Format.fprintf fmt "%s%t" pname k in
  Ctypes_static.view ~format_typ ~read ~write typ

external to_voidp : nativeint -> Cstubs_internals.voidp = "%identity"

let invalid_code () = failwith "ppx_cstub generated invalid code"

module Signed = struct
  module Nativeint = struct
    include struct
      [@@@ocaml.warning "-32"]

      let equal (x : nativeint) (y : nativeint) = x = y

      let pp fmt x = Format.fprintf fmt "%nd" x
    end

    include Nativeint

    module Infix = struct
      let ( + ) = add

      let ( - ) = sub

      let ( * ) = mul

      let ( / ) = div

      let ( mod ) = rem

      let ( land ) = logand

      let ( lor ) = logor

      let ( lxor ) = logxor

      let ( lsl ) = shift_left

      let ( lsr ) = shift_right_logical

      [@@@ocaml.warning "-32"]

      let ( asr ) = shift_right
    end

    external of_nativeint : t -> t = "%identity"

    external to_nativeint : t -> t = "%identity"

    let of_int64 = Int64.to_nativeint

    let to_int64 = Int64.of_nativeint

    [@@@ocaml.warning "-32"]

    let max = max

    let min = min
  end

  module type Int_size = sig
    val int_size : int
  end

  module Short_int (X : Int_size) = struct
    open X

    type t = int

    let land_mask = (1 lsl int_size) - 1

    let cor = (1 lsl int_size) * -1

    let min_int = cor / 2

    let max_int = land_mask / 2

    let sign_bit = int_size - 1

    let of_int x =
      let res = x land land_mask in
      (cor * (res lsr sign_bit)) + res

    let add x y = of_int (x + y)

    let sub x y = of_int (x - y)

    let mul x y = of_int (x * y)

    let div x y = of_int (x / y)

    let rem x y = x mod y

    let logand x y = x land y

    let logor x y = x lor y

    let logxor x y = x lxor y

    let shift_left x y = of_int (x lsl y)

    let shift_right_logical x y =
      of_int ((x lsr y) land ((1 lsl (int_size - y)) - 1))

    let shift_right x y = of_int (x asr y)

    module Infix = struct
      let ( + ) = add

      let ( - ) = sub

      let ( * ) = mul

      let ( / ) = div

      let ( mod ) = rem

      let ( land ) = logand

      let ( lor ) = logor

      let ( lxor ) = logxor

      let ( lsl ) = shift_left

      let ( lsr ) = shift_right_logical

      [@@@ocaml.warning "-32"]

      let ( asr ) = shift_right
    end

    let lognot x = lnot x

    let compare = compare

    external to_int : t -> t = "%identity"

    let of_string x =
      let r = int_of_string x in
      if r < min_int || r > max_int then failwith "int_of_string";
      r

    let to_string = string_of_int

    let zero = 0

    let one = 1

    let minus_one = -1

    let succ x = of_int (succ x)

    let pred x = of_int (pred x)

    let to_int64 = Int64.of_int

    let of_int64 x = of_int (Int64.to_int x)

    let to_nativeint = Nativeint.of_int

    let of_nativeint x = of_int (Nativeint.to_int x)

    let abs x = of_int (abs x)

    let neg x = of_int (-x)

    [@@@ocaml.warning "-32"]

    let max = max

    let min = min

    let equal (x : t) (y : t) = x = y

    let pp fmt n = Format.fprintf fmt "%d" n
  end

  module Int8 = Short_int (struct
    let int_size = 8
  end)

  module Int16 = Short_int (struct
    let int_size = 16
  end)

  module Int32 = Short_int (struct
    let int_size = 32
  end)

  module type Short = sig
    include Signed.S with type t = int
  end

  module Short = ( val match Ctypes.sizeof Ctypes.short with
                       | 1 -> (module Int8)
                       | 2 -> (module Int16)
                       | 4 when Ctypes.sizeof Ctypes.int = 4 ->
                         if Sys.word_size = 64 then (module Int32)
                         else (module Signed.Int)
                       | _ -> failwith "invalid size of short" : Short )

  module Schar = Int8
end

module Callback = struct
  module type Info = sig
    type real

    val real : real Ctypes.fn
  end

  module Make (H : Info) : sig
    type fn = H.real

    type 'a t

    type raw_pointer

    val t : H.real t Ctypes.static_funptr Ctypes.typ

    val fn : H.real Ctypes.fn

    val make_pointer : raw_pointer -> H.real t Ctypes.static_funptr
  end = struct
    open H

    type fn = real

    type 'a t = real

    type raw_pointer = nativeint

    let t = Ctypes.static_funptr real

    let fn = H.real

    let make_pointer p = Cstubs_internals.make_fun_ptr H.real (to_voidp p)
  end

  let make (type a) (fn : a Ctypes.fn) =
    ( module struct
      type real = a

      let real = fn
    end : Info
      with type real = a )
end

module Shadow = struct
  let rec passable : type a. a typ -> bool = function
    | Void -> true
    | Primitive _ -> true
    | Struct { spec = Incomplete _; _ } -> raise IncompleteType
    | Struct { spec = Complete _; _ } -> true
    | Union { uspec = None; _ } -> raise IncompleteType
    | Union { uspec = Some _; _ } -> true
    | Array _ -> false
    | Bigarray _ -> false
    | Pointer _ -> true
    | Funptr _ -> true
    (* Allow to pass and return abstract types. I don't know why it is
       disabled upstream. They are handled like structs and unions *)
    | Abstract _ -> true
    | OCaml _ -> true
    | View { ty; _ } -> passable ty

  let ( @-> ) a b =
    if not (passable a) then raise (Unsupported "Unsupported argument type")
    else Function (a, b)

  let returning a =
    if not (passable a) then raise (Unsupported "Unsupported return type")
    else Returns a
end

external obj_magic : 'a -> 'b = "%identity"
