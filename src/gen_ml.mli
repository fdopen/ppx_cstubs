open Migrate_parsetree.Ast_405
open Parsetree

type result = {
  extern: structure_item;
  intern: structure_item;
}

val external' :
  'a Ctypes.fn ->
  string ->
  (Asttypes.arg_label * expression) list ->
  expression ->
  Gen_c.info ->
  result

val foreign :
  'a Ctypes.fn ->
  string ->
  Gen_c.info ->
  expression ->
  result

val foreign_value :
  'a Ctypes.fn ->
  string ->
  expression ->
  Gen_c.info ->
  result
