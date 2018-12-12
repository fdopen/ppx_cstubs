(* not necessary, but triggers all fatal exceptions *)
val init : unit -> unit

val word_size : unit -> int
val ext_obj : unit -> string
val version : unit -> int * int * int
val system : unit -> string

val runtime_version : int * int * int
