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
     't Ctypes_static.typ
  -> string
  -> int
  -> 'a Ctypes_static.typ
  -> ('a, 't) Ctypes_static.field

external to_voidp : nativeint -> Cstubs_internals.voidp = "%identity"

val invalid_code : unit -> 'a

val build_enum :
     string
  -> 'a Ctypes.typ
  -> typedef:bool
  -> ?unexpected:(int64 -> 'b)
  -> ('b * 'a) list
  -> 'b Ctypes.typ

val build_enum_bitmask :
     string
  -> 'a Ctypes.typ
  -> typedef:bool
  -> ('b * 'a) list
  -> 'b list Ctypes.typ

val string_write : string -> char Ctypes_static.ptr

val string_read : nativeint -> string

val string_opt_read : nativeint -> string option

val string_opt_write : string option -> char Ctypes_static.ptr

(**/**)
