val htl_expr :
  (Marshal_types.id, Migrate_parsetree.Ast_405.Parsetree.expression)
    Hashtbl.t
val htl_stri :
  (Marshal_types.id, Migrate_parsetree.Ast_405.Parsetree.structure_item)
    Hashtbl.t
val htl_used : (int, unit) Hashtbl.t
val foreign_used : bool ref
val c_source : string option ref
val clear : unit -> unit
