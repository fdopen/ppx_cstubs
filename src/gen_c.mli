type info = {
  stub_source: string;
  stub_name: string;
  stub_name_byte: string option;
  noalloc: bool;
  return_errno: bool;
}

val gen_fun: 'a Ctypes.fn ->
  stubname:string ->
  cfunc:string ->
  release_runtime_lock:bool ->
  noalloc:bool ->
  return_errno:bool ->
  info

val gen_value: 'a Ctypes.fn -> stubname:string -> value:string -> info

val string_of_typ_exn : ?name:string -> 'a Ctypes.typ -> string

val build_inline_fun :
  'a Ctypes.fn ->
  c_name:string ->
  c_body:string ->
  noalloc:bool ->
  (string * string) list
  -> string

(* fixme: MOVE*)
val is_void : 'a Ctypes.typ -> bool
