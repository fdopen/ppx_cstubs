type extr_info

type info = {
  signed : bool;
  min : string option;
  max : string option;
  info : extr_info;
}

val prepare : 'a Ctypes_static.typ -> info option
val gen : info -> string -> Ast_405.Parsetree.expression option
