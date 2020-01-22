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

(* These functions are used by generated code, you should NEVER use them
   manually. *)

(**/**)

val seal : 'a Ctypes_static.typ -> size:int -> align:int -> unit

val add_field :
  't Ctypes_static.typ ->
  string ->
  int ->
  'a Ctypes_static.typ ->
  ('a, 't) Ctypes_static.field

external to_voidp : nativeint -> Cstubs_internals.voidp = "%identity"

external identity : 'a -> 'a = "%identity"

val invalid_code : unit -> 'a

val build_enum :
  string ->
  'a Ctypes.typ ->
  typedef:bool ->
  ?unexpected:(int64 -> 'b) ->
  ('b * 'a) list ->
  'b Ctypes.typ

val build_enum_bitmask :
  string ->
  'a Ctypes.typ ->
  typedef:bool ->
  ?unexpected:('b list -> int64 -> 'b list) ->
  ('b * 'a) list ->
  'b list Ctypes.typ

module Signed : sig
  module Nativeint : Signed.S with type t = nativeint

  module Int8 : Signed.S with type t = int

  module Int16 : Signed.S with type t = int

  module Int32 : Signed.S with type t = int

  module Schar : Signed.S with type t = int

  module Short : Signed.S with type t = int
end

module Callback : sig
  module type Info = sig
    type real

    val real : real Ctypes_static.fn
  end

  module Make (H : Info) : sig
    type fn = H.real

    type 'a t

    type raw_pointer

    val t : H.real t Ctypes.static_funptr Ctypes.typ

    val fn : fn Ctypes_static.fn

    val make_pointer : raw_pointer -> H.real t Ctypes.static_funptr
  end

  val make : 'a Ctypes.fn -> (module Info with type real = 'a)

  (*
  always two hops to prune `real` away for nicer type hints:

  module E = Make ((val make (int @-> returning int)))
  *)
end

module Shadow : sig
  val ( @-> ) : 'a Ctypes.typ -> 'b Ctypes.fn -> ('a -> 'b) Ctypes.fn

  val returning : 'a Ctypes.typ -> 'a Ctypes.fn
end

external fatptr_magic : _ Cstubs_internals.fatptr -> _ Cstubs_internals.fatptr
  = "%identity"

external fatfunptr_magic :
  _ Cstubs_internals.fatfunptr -> _ Cstubs_internals.fatfunptr = "%identity"

(**/**)
