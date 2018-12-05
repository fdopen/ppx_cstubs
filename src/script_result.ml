let htl_expr = Hashtbl.create 64
let htl_used = Hashtbl.create 64
let htl_stri = Hashtbl.create 16

let foreign_used = ref false
let c_source = ref None

let clear () =
  Hashtbl.clear htl_expr;
  Hashtbl.clear htl_stri;
  Hashtbl.clear htl_used;
  foreign_used := false;
  c_source := None
