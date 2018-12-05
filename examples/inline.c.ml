let%c () = header {|
#include <stdio.h>
|}

external%c print: str:string -> d:int -> void = {|
  printf("%s (%d)\n",$str,$d);
  fflush(stdout);
|}

(* labels that end with an underscore, will disappear
   in the generated OCaml function. They are always removed,
   if an operator is defined. *)
external%c printnl: str_:string -> d_:int -> void = {|
  fprintf(stderr,"%s (%d)\n",$str_,$d_);
  /* $$ to write one regular dollar sign */

  fflush(stderr);
|}

let () =
  print ~str:"Hello World" ~d:3;
  printnl "Hello World (nl)" 9
