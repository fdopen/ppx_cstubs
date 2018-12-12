type 'a return = { return : 'b. 'a -> 'b; }
val with_return : ('a return -> 'a) -> 'a
val finally : h:(unit -> unit) -> (unit -> 'a) -> 'a
external identity : 'a -> 'a = "%identity"
module Util :
  sig
    open Migrate_parsetree.Ast_405
    val error : ?loc:Ast_helper.loc ->
      ('a, unit, string, 'b) format4 -> 'a
    val safe_ascii_only : string -> string
    val safe_ascii_only_ml : string -> string
    val safe_cname : prefix:string -> string
    val safe_mlname : ?prefix:string -> unit -> string
    val with_loc : Ast_helper.loc -> (unit -> 'a) -> 'a
    val str_expr : ?loc:Ast_helper.loc ->
      string -> Parsetree.expression
    val int_expr :
      ?loc:Ast_helper.loc ->
      ?attrs:Ast_helper.attrs -> int -> Parsetree.expression

    val mk_loc :  'a -> 'a Location.loc

    val mk_lid: ?loc:Ast_helper.loc -> string -> Longident.t Location.loc
    val mk_pat: string -> Parsetree.pattern
  end
