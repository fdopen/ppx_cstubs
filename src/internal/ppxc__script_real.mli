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
module Run_environment : sig
  module Libffi_abi : sig
    type abi

    val aix : abi

    val darwin : abi

    val eabi : abi

    val fastcall : abi

    val gcc_sysv : abi

    val linux : abi

    val linux64 : abi

    val linux_soft_float : abi

    val ms_cdecl : abi

    val n32 : abi

    val n32_soft_float : abi

    val n64 : abi

    val n64_soft_float : abi

    val o32 : abi

    val o32_soft_float : abi

    val osf : abi

    val pa32 : abi

    val stdcall : abi

    val sysv : abi

    val thiscall : abi

    val unix : abi

    val unix64 : abi

    val v8 : abi

    val v8plus : abi

    val v9 : abi

    val vfp : abi

    val default_abi : abi
  end

  module Foreign : sig
    val funptr :
      ?abi:Libffi_abi.abi ->
      ?name:string ->
      ?check_errno:bool ->
      ?runtime_lock:bool ->
      ?thread_registration:bool ->
      ('a -> 'b) Ctypes.fn ->
      ('a -> 'b) Ctypes.typ

    val funptr_opt :
      ?abi:Libffi_abi.abi ->
      ?name:string ->
      ?check_errno:bool ->
      ?runtime_lock:bool ->
      ?thread_registration:bool ->
      ('a -> 'b) Ctypes.fn ->
      ('a -> 'b) option Ctypes.typ
  end

  val funptr :
    ?abi:Libffi_abi.abi ->
    ?name:string ->
    ?check_errno:bool ->
    ?runtime_lock:bool ->
    ?thread_registration:bool ->
    ('a -> 'b) Ctypes.fn ->
    ('a -> 'b) Ctypes.typ

  val funptr_opt :
    ?abi:Libffi_abi.abi ->
    ?name:string ->
    ?check_errno:bool ->
    ?runtime_lock:bool ->
    ?thread_registration:bool ->
    ('a -> 'b) Ctypes.fn ->
    ('a -> 'b) option Ctypes.typ

  module Ctypes_static : sig end

  module Ctypes_primitive_types : sig end

  module Ctypes_printers : sig end

  module Ctypes_structs : sig end

  module Ctypes_types : sig end
end

module Ctypes_make : sig
  module type Signed_type = sig
    include Signed.S

    val t : t Ctypes_static.typ
  end

  module type Unsigned_type = sig
    include Unsigned.S

    val t : t Ctypes_static.typ
  end

  module type Target_env = sig
    module Uintptr : Unsigned_type

    module Intptr : Signed_type

    module Ptrdiff : Signed_type
  end

  module Info : Target_env

  module Info32 : Target_env

  module Info64 : Target_env

  module Ctypes (T : Target_env) : sig
    type ('a, 'b) pointer = ('a, 'b) Ctypes_static.pointer

    type 'a ptr = ('a, [ `C ]) pointer

    type 'a ocaml = 'a Ctypes_static.ocaml

    type 'a carray = 'a Ctypes_static.carray

    type 'a bigarray_class = 'a Ctypes_static.bigarray_class

    val genarray :
      < ba_repr : 'b
      ; bigarray : ('a, 'b, 'l) Bigarray.Genarray.t
      ; carray : 'a carray
      ; dims : int array
      ; element : 'a
      ; layout : 'l >
      bigarray_class

    val array1 :
      < ba_repr : 'b
      ; bigarray : ('a, 'b, 'l) Bigarray.Array1.t
      ; carray : 'a carray
      ; dims : int
      ; element : 'a
      ; layout : 'l >
      bigarray_class

    val array2 :
      < ba_repr : 'b
      ; bigarray : ('a, 'b, 'l) Bigarray.Array2.t
      ; carray : 'a carray carray
      ; dims : int * int
      ; element : 'a
      ; layout : 'l >
      bigarray_class

    val array3 :
      < ba_repr : 'b
      ; bigarray : ('a, 'b, 'l) Bigarray.Array3.t
      ; carray : 'a carray carray carray
      ; dims : int * int * int
      ; element : 'a
      ; layout : 'l >
      bigarray_class

    type ('a, 'kind) structured = ('a, 'kind) Ctypes_static.structured

    type 'a structure = ('a, [ `Struct ]) structured

    type 'a union = ('a, [ `Union ]) structured

    type ('a, 't) field = ('a, 't) Ctypes_static.field

    type 'a abstract = 'a Ctypes_static.abstract

    type 'a typ = 'a Ctypes_static.typ

    val void : unit typ

    val char : char typ

    val schar : int typ

    val short : int typ

    val int : int typ

    val long : Signed.long typ

    val llong : Signed.llong typ

    val nativeint : nativeint typ

    val int8_t : int typ

    val int16_t : int typ

    val int32_t : int32 typ

    val int64_t : int64 typ

    module Intptr : Signed.S

    val intptr_t : Intptr.t typ

    module Ptrdiff : Signed.S

    val ptrdiff_t : Ptrdiff.t typ

    val camlint : int typ

    val uchar : Unsigned.uchar typ

    val bool : bool typ

    val uint8_t : Unsigned.uint8 typ

    val uint16_t : Unsigned.uint16 typ

    val uint32_t : Unsigned.uint32 typ

    val uint64_t : Unsigned.uint64 typ

    val size_t : Unsigned.size_t typ

    val ushort : Unsigned.ushort typ

    val sint : Signed.sint typ

    val uint : Unsigned.uint typ

    val ulong : Unsigned.ulong typ

    val ullong : Unsigned.ullong typ

    module Uintptr : Unsigned.S

    val uintptr_t : Uintptr.t typ

    val float : float typ

    val double : float typ

    val ldouble : LDouble.t typ

    val complex32 : Complex.t typ

    val complex64 : Complex.t typ

    val complexld : ComplexL.t typ

    val ptr : 'a typ -> 'a Ctypes_static.ptr typ

    val ptr_opt : 'a typ -> 'a Ctypes_static.ptr option typ

    val string : string typ

    val string_opt : string option typ

    val ocaml_string : string Ctypes_static.ocaml typ

    val ocaml_bytes : Bytes.t Ctypes_static.ocaml typ

    val array : int -> 'a typ -> 'a Ctypes_static.carray typ

    val bigarray :
      < ba_repr : 'b
      ; bigarray : 'bigarray
      ; carray : 'c
      ; dims : 'dims
      ; element : 'a
      ; layout : Bigarray.c_layout >
      Ctypes_static.bigarray_class ->
      'dims ->
      ('a, 'b) Bigarray.kind ->
      'bigarray typ

    val fortran_bigarray :
      < ba_repr : 'b
      ; bigarray : 'bigarray
      ; carray : 'c
      ; dims : 'dims
      ; element : 'a
      ; layout : Bigarray.fortran_layout >
      Ctypes_static.bigarray_class ->
      'dims ->
      ('a, 'b) Bigarray.kind ->
      'bigarray typ

    val typ_of_bigarray_kind : ('a, 'b) Bigarray.kind -> 'a typ

    (* missing:
       val structure : string -> 's Ctypes_static.structure typ
       val union : string -> 's Ctypes_static.union typ
       val field : ('s, [< `Struct | `Union ] as 'b)
       Ctypes_static.structured typ -> string -> 'a typ -> ('a, ('s, 'b)
       Ctypes_static.structured) field val seal : ('a, [< `Struct | `Union ])
       Ctypes_static.structured typ -> unit *)
    val view :
      ?format_typ:((Format.formatter -> unit) -> Format.formatter -> unit) ->
      ?format:(Format.formatter -> 'b -> unit) ->
      read:('a -> 'b) ->
      write:('b -> 'a) ->
      'a typ ->
      'b typ

    val typedef : 'a typ -> string -> 'a typ

    (* missing: val abstract : name:string -> size:int -> alignment:int -> 'a
       Ctypes_static.abstract typ val lift_typ : 'a Ctypes_static.typ -> 'a typ *)
    type 'a fn = 'a Ctypes_static.fn

    val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn

    val returning : 'a typ -> 'a fn

    type 'a static_funptr = 'a Ctypes_static.static_funptr

    val static_funptr : 'a fn -> 'a Ctypes_static.static_funptr typ

    val sizeof : 'a typ -> int

    (* runtime fail *)
    val alignment : 'a typ -> int

    (* runtime fail *)
    val format_typ : ?name:string -> Format.formatter -> 'a typ -> unit

    val format_fn : ?name:string -> Format.formatter -> 'a fn -> unit

    val string_of_typ : ?name:string -> 'a typ -> string

    val string_of_fn : ?name:string -> 'a fn -> string

    val format : 'a typ -> Format.formatter -> 'a -> unit

    (* runtime fail *)
    val string_of : 'a typ -> 'a -> string

    (* runtime fail *)
    val null : unit ptr

    val ( !@ ) : 'a ptr -> 'a

    val ( <-@ ) : 'a ptr -> 'a -> unit

    val ( +@ ) : ('a, 'b) pointer -> int -> ('a, 'b) pointer

    val ( -@ ) : ('a, 'b) pointer -> int -> ('a, 'b) pointer

    val ptr_diff : ('a, 'b) pointer -> ('a, 'b) pointer -> int

    val from_voidp : 'a typ -> unit ptr -> 'a ptr

    val to_voidp : 'a ptr -> unit ptr

    val allocate : ?finalise:('a ptr -> unit) -> 'a typ -> 'a -> 'a ptr

    val allocate_n : ?finalise:('a ptr -> unit) -> 'a typ -> count:int -> 'a ptr

    val ptr_compare : 'a ptr -> 'a ptr -> int

    val is_null : 'a ptr -> bool

    val reference_type : 'a ptr -> 'a typ

    val ptr_of_raw_address : nativeint -> unit ptr

    val funptr_of_raw_address :
      nativeint -> (unit -> unit) Ctypes_static.static_funptr

    val raw_address_of_ptr : unit ptr -> nativeint

    val string_from_ptr : char ptr -> length:int -> string

    val ocaml_string_start : string -> string ocaml

    val ocaml_bytes_start : Bytes.t -> Bytes.t ocaml

    module CArray : sig
      include module type of Ctypes.CArray
    end

    val bigarray_start :
      < ba_repr : 'c
      ; bigarray : 'b
      ; carray : 'd
      ; dims : 'e
      ; element : 'a
      ; layout : 'l >
      bigarray_class ->
      'b ->
      'a ptr

    val bigarray_of_ptr :
      < ba_repr : 'f
      ; bigarray : 'b
      ; carray : 'c
      ; dims : 'i
      ; element : 'a
      ; layout : Bigarray.c_layout >
      bigarray_class ->
      'i ->
      ('a, 'f) Bigarray.kind ->
      'a ptr ->
      'b

    val fortran_bigarray_of_ptr :
      < ba_repr : 'f
      ; bigarray : 'b
      ; carray : 'c
      ; dims : 'i
      ; element : 'a
      ; layout : Bigarray.fortran_layout >
      bigarray_class ->
      'i ->
      ('a, 'f) Bigarray.kind ->
      'a ptr ->
      'b

    val array_of_bigarray :
      < ba_repr : 'a
      ; bigarray : 'b
      ; carray : 'c
      ; dims : 'd
      ; element : 'e
      ; layout : Bigarray.c_layout >
      bigarray_class ->
      'b ->
      'c

    val bigarray_of_array :
      < ba_repr : 'f
      ; bigarray : 'b
      ; carray : 'c carray
      ; dims : 'i
      ; element : 'a
      ; layout : Bigarray.c_layout >
      bigarray_class ->
      ('a, 'f) Bigarray.kind ->
      'c carray ->
      'b

    val make :
      ?finalise:(('a, 'b) structured -> unit) ->
      ('a, 'b) structured typ ->
      ('a, 'b) structured

    val setf :
      ('b, 'c) structured -> ('a, ('b, 'c) structured) field -> 'a -> unit

    val getf : ('b, 'c) structured -> ('a, ('b, 'c) structured) field -> 'a

    val ( @. ) :
      ('b, 'c) structured -> ('a, ('b, 'c) structured) field -> 'a ptr

    val ( |-> ) :
      ('b, 'c) structured ptr -> ('a, ('b, 'c) structured) field -> 'a ptr

    val offsetof : ('a, 'b structure) field -> int

    (* runtime fail *)
    val field_type : ('a, 'b) field -> 'a typ

    val field_name : ('a, 'b) field -> string

    val addr : ('a, 'b) structured -> ('a, 'b) structured ptr

    val coerce : 'a typ -> 'b typ -> 'a -> 'b

    val coerce_fn : 'a fn -> 'b fn -> 'a -> 'b

    (* private. Don't use. Only available during code generation *)

    val ppxc__private_ocaml_typ :
      string -> [ `priv ] Ctypes_static.structure Ctypes.typ

    val ppxc__private_structure : string -> 's Ctypes_static.structure typ

    val ppxc__private_union : string -> 's Ctypes_static.union typ

    val ppxc__unavailable : string -> 'a
  end
end

(* everything below is private. Don't use it *)

module type Opaque_result = sig
  type t

  val t : t Ctypes.typ
end

module Extract_phase0 : sig
  val bound_constant : string -> string -> unit

  val header : string -> unit

  val int_alias : string -> string -> unit
end

module Extract : sig
  val constant : string -> string -> string -> 'a Ctypes.typ -> unit

  val bound_constant : string -> string -> string -> 'a Ctypes.typ -> 'a

  val register_fun_place : string -> unit

  val header : string -> unit

  val field :
    string ->
    't Ctypes.typ ->
    string ->
    'a Ctypes.typ ->
    ( 'a,
      (('s, [< `Struct | `Union ]) Ctypes_static.structured as 't) )
    Ctypes_static.field

  val seal :
    string ->
    string ->
    ('a, [< `Struct | `Union ]) Ctypes_static.structured Ctypes_static.typ ->
    unit

  val enum : 'a list -> string -> 'a Ctypes.typ * 'a list Ctypes.typ

  val int_alias :
    string ->
    mod_name:string ->
    ?strict:bool ->
    string ->
    (module Ppx_cstubs.Types.Signed)

  val uint_alias :
    string ->
    mod_name:string ->
    ?strict:bool ->
    string ->
    (module Ppx_cstubs.Types.Unsigned)

  val aint_alias :
    string ->
    mod_name:string ->
    ?strict:bool ->
    string ->
    (module Ppx_cstubs.Types.Unkown_signedness)

  val opaque :
    size:string ->
    align:string ->
    typ:string ->
    mi:string ->
    string ->
    (module Opaque_result)

  val abstract :
    size:string ->
    align:string ->
    string ->
    'a Ctypes_static.abstract Ctypes_static.typ

  val ocaml_funptr :
    string -> 'a Ctypes_static.static_funptr Ctypes_static.typ -> unit
end

module Build : sig
  val reg_trace_fn : int -> 'a Ctypes.fn -> 'a Ctypes.fn

  val reg_trace : ?no_dup:bool -> int -> 'a Ctypes.typ -> 'a Ctypes.typ

  val constant : string -> string -> string -> 'a Ctypes.typ -> unit

  val bound_constant : string -> string -> string -> 'a Ctypes.typ -> 'a

  val foreign_value :
    string -> string -> 'a Ctypes.typ -> string -> string -> string -> unit

  val external' :
    string -> string -> 'a Ctypes.fn -> marshal_info:string -> unit

  val enum : 'a list -> string -> 'a Ctypes.typ * 'a list Ctypes.typ

  val foreign :
    string ->
    string ->
    ocaml_name:string ->
    typ_expr:string ->
    ?release_runtime_lock:bool ->
    ?noalloc:bool ->
    ?return_errno:bool ->
    string ->
    'a Ctypes.fn ->
    unit

  val field :
    string ->
    't Ctypes.typ ->
    string ->
    'a Ctypes.typ ->
    ( 'a,
      (('s, [< `Struct | `Union ]) Ctypes_static.structured as 't) )
    Ctypes_static.field

  val int_alias :
    string ->
    mod_name:string ->
    ?strict:bool ->
    string ->
    (module Ppx_cstubs.Types.Signed)

  val uint_alias :
    string ->
    mod_name:string ->
    ?strict:bool ->
    string ->
    (module Ppx_cstubs.Types.Unsigned)

  val aint_alias :
    string ->
    mod_name:string ->
    ?strict:bool ->
    string ->
    (module Ppx_cstubs.Types.Unkown_signedness)

  val opaque :
    size:string ->
    align:string ->
    typ:string ->
    mi:string ->
    string ->
    (module Opaque_result)

  val abstract :
    size:string ->
    align:string ->
    string ->
    'a Ctypes_static.abstract Ctypes_static.typ

  val trace_custom : 'a Ctypes_static.typ -> 'a Ctypes_static.typ

  val ocaml_funptr :
    string -> 'a Ctypes_static.static_funptr Ctypes_static.typ -> unit

  val seal :
    string ->
    string ->
    ('a, [< `Struct | `Union ]) Ctypes_static.structured Ctypes_static.typ ->
    unit
end

module Main : sig
  val run : unit -> unit
end

val set_loc : string -> unit

val _init : unit -> unit
