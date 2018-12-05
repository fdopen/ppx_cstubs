(* not necessary, but triggers all fatal exceptions *)
val init : unit -> unit

val word_size : unit -> int
val ext_obj : unit -> string
val version : unit -> int * int * int
val dev_null : string
