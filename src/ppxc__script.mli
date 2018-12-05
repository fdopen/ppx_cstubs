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
      ('a -> 'b) Ctypes.fn -> ('a -> 'b) Ctypes.typ

    val funptr_opt :
      ?abi:Libffi_abi.abi ->
      ?name:string ->
      ?check_errno:bool ->
      ?runtime_lock:bool ->
      ?thread_registration:bool ->
      ('a -> 'b) Ctypes.fn -> ('a -> 'b) option Ctypes.typ
  end

  val funptr :
    ?abi:Libffi_abi.abi ->
    ?name:string ->
    ?check_errno:bool ->
    ?runtime_lock:bool ->
    ?thread_registration:bool ->
    ('a -> 'b) Ctypes.fn -> ('a -> 'b) Ctypes.typ

  val funptr_opt :
    ?abi:Libffi_abi.abi ->
    ?name:string ->
    ?check_errno:bool ->
    ?runtime_lock:bool ->
    ?thread_registration:bool ->
      ('a -> 'b) Ctypes.fn -> ('a -> 'b) option Ctypes.typ

  module Ctypes_static : sig  end

  module Ctypes_primitive_types : sig  end

  module Ctypes_printers : sig  end

  module Ctypes_structs : sig  end

  module Ctypes_types : sig  end
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

  module type Target_env =
  sig
    module Uintptr : Unsigned_type
    module Intptr : Signed_type
    module Ptrdiff : Signed_type
  end

  module Info : Target_env
  module Info32 : Target_env
  module Info64 : Target_env

  module Ctypes : functor (T : Target_env) -> sig
    type ('a, 'kind) structured = ('a, 'kind) Ctypes_static.structured
    type 'a structure = ('a, [ `Struct ]) structured
    type 'a union = ('a, [ `Union ]) structured
    type ('a, 't) field = ('a, 't) Ctypes_static.field
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
    val float : float typ
    val double : float typ
    val ldouble : LDouble.t typ
    val complex32 : Complex.t typ
    val complex64 : Complex.t typ
    val complexld : ComplexL.t typ
    val string : string typ
    val string_opt : string option typ
    val ocaml_string : string Ctypes_static.ocaml typ
    val ocaml_bytes : Bytes.t Ctypes_static.ocaml typ
    val bigarray :
      < ba_repr : 'b; bigarray : 'bigarray; carray : 'c; dims : 'dims;
        element : 'a; layout : Bigarray.c_layout >
        Ctypes_static.bigarray_class ->
      'dims -> ('a, 'b) Bigarray.kind -> 'bigarray typ
    val fortran_bigarray :
      < ba_repr : 'b; bigarray : 'bigarray; carray : 'c; dims : 'dims;
        element : 'a; layout : Bigarray.fortran_layout >
        Ctypes_static.bigarray_class ->
      'dims -> ('a, 'b) Bigarray.kind -> 'bigarray typ
    val genarray :
      < ba_repr : 'b; bigarray : ('a, 'b, 'l) Bigarray.Genarray.t;
        carray : 'a Ctypes_static.carray; dims : int array;
        element : 'a; layout : 'l >
        Ctypes_static.bigarray_class
    val array1 :
      < ba_repr : 'b; bigarray : ('a, 'b, 'l) Bigarray.Array1.t;
        carray : 'a Ctypes_static.carray; dims : int; element : 'a;
        layout : 'l >
        Ctypes_static.bigarray_class
    val array2 :
      < ba_repr : 'b; bigarray : ('a, 'b, 'l) Bigarray.Array2.t;
        carray : 'a Ctypes_static.carray Ctypes_static.carray;
        dims : int * int; element : 'a; layout : 'l >
        Ctypes_static.bigarray_class
    val array3 :
      < ba_repr : 'b; bigarray : ('a, 'b, 'l) Bigarray.Array3.t;
        carray : 'a Ctypes_static.carray Ctypes_static.carray
            Ctypes_static.carray;
        dims : int * int * int; element : 'a; layout : 'l >
        Ctypes_static.bigarray_class
    val typ_of_bigarray_kind : ('a, 'b) Bigarray.kind -> 'a typ
    val structure : string -> 's Ctypes_static.structure typ
    val union : string -> 's Ctypes_static.union typ
    type 'a fn = 'a Ctypes_static.fn
    type 'a static_funptr = 'a Ctypes_static.static_funptr
    module Uintptr : Unsigned.S
    val uintptr_t : Uintptr.t typ
    module Ptrdiff : Signed.S
    val ptrdiff_t : Ptrdiff.t typ
    module Intptr : Signed.S
    val intptr_t : Intptr.t typ
    val static_funptr : 'a fn -> 'a Ctypes_static.static_funptr typ
    val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn
    val returning : 'a typ -> 'a fn
    val typedef : 'a typ -> string -> 'a typ
    val view :
      ?format_typ:((Format.formatter -> unit) ->
                   Format.formatter -> unit) ->
      ?format:(Format.formatter -> 'b -> unit) ->
      read:('a -> 'b) ->
      write:('b -> 'a) -> 'a typ -> 'b typ
    val array : int -> 'a typ -> 'a Ctypes_static.carray typ
    val ptr : 'a typ -> 'a Ctypes_static.ptr typ
    val ptr_opt : 'a typ -> 'a Ctypes_static.ptr option typ

    (* private. Don't use *)
    val ppxc__private_field :
      ('s, [< `Struct | `Union ] as 'b) Ctypes_static.structured typ ->
      string -> 'a typ -> ('a, ('s, 'b) Ctypes_static.structured) field
    val ppxc__private_seal :
      ('a, [< `Struct | `Union ]) Ctypes_static.structured typ -> unit
    val ppxc__private_make :
      ?finalise:('s -> unit) -> ((_, _) structured as 's) typ -> 's
    val ppxc__private_setf :
      ((_, _) structured as 's) -> ('a, 's) field -> 'a -> unit
    val ppxc__private_getf :
      ((_, _) structured as 's) -> ('a, 's) field -> 'a
    val ppxc__private_ocaml_typ:
      string -> [ `priv ] Ctypes_static.structure Ctypes.typ
  end
end

(* everything below is private. Don't use it *)
module Extract : sig
  val constant : string -> string -> 'a Ctypes.typ -> unit
  val register_fun_place : string -> unit
  val header : string -> unit
  val field : string -> 'a Ctypes.typ -> string -> unit
  val seal : string -> string -> 'a Ctypes.typ -> unit
  val enum :
    string -> ?typedef:bool -> string -> [> `A ] Cstubs_internals.typ
  val enum2 : 'a list -> string -> 'a Cstubs_internals.typ
end

module Build : sig
  val reg_trace_fn : int -> 'a Ctypes.fn -> 'a Ctypes.fn
  val reg_trace : int -> 'a Ctypes.typ -> 'a Ctypes.typ
  val constant : string -> string -> 'a Ctypes.typ -> 'a
  val foreign_value :
    string -> string -> 'a Ctypes.typ -> string -> string -> string -> unit

  val external' : string -> string ->
    'a Ctypes.fn ->
    marshal_info:string -> unit

  val enum :
    string ->
    string ->
    ?typedef:bool ->
    ?unexpected:(int64 -> 'a) ->
    ('a * int64) list -> 'a Cstubs_internals.typ

  val enum2 : 'a list -> string -> 'a Cstubs_internals.typ

  val foreign :
    string ->
    string ->
    ocaml_name:string ->
    typ_expr:string ->
    ?release_runtime_lock:bool ->
    ?noalloc:bool -> ?return_errno:bool -> string -> 'a Ctypes.fn -> unit
end

module Main : sig
  val run : unit -> unit
end

val set_loc : string -> unit
val _init : unit -> unit
