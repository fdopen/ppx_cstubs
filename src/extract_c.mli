type id
type intern

type extract_info = {
  id : id;
  single_prog : string;
  intern : intern;
}

val prologue : string

(* buf can be used to extract several values
   from one object file. User must add prologue himself manually
   before the first call *)
val prepare_extract_int :
  ?min:string ->
  ?max:string ->
  buf:Buffer.t ->
  c_header:string -> expr:string -> signed:bool -> unit -> extract_info

val prepare_extract_string :
  buf:Buffer.t -> c_header:string -> expr:string -> unit -> extract_info

type obj

val compile : string -> (obj, string) result

type extract_error =
  | Info_not_found
  | Overflow
  | Underflow
  | User_overflow
  | User_underflow
  | Not_an_integer

val extract : extract_info -> obj -> (string, extract_error) result
