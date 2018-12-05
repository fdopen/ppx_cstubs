let h s =
  let x = Location.mkloc s Location.none in
  (x,Migrate_parsetree.Ast_405.Parsetree.PStr [])

let replace_expr_string = "ppxc__replace_expr"
let replace_expr_attrib = h replace_expr_string

let tdl_string = "ppxc__tdl"
let tdl_attrib = h tdl_string

let remove_string = "ppxc__remove"
let remove_attrib = h remove_string

let replace_attr_string = "ppxc__replace_attr"

