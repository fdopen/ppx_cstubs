open Migrate_parsetree.Ast_405

val replace_expr_string : string
val replace_expr_attrib : string Location.loc * Parsetree.payload

val tdl_string : string
val tdl_attrib : string Location.loc * Parsetree.payload

val remove_string : string
val remove_attrib : string Location.loc * Parsetree.payload

val replace_attr_string : string
