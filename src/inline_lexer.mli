exception Bad_expander
type t =
  | Literal of string
  | Variable of (string * Lexing.position * Lexing.position)
  | Textend
val token : Lexing.lexbuf -> t
