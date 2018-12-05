let%c () = header {|
#include <ftw.h>
|}

(*
c prototype:
int ftw(const char *dirpath,
        int ( *fn ) (const char *fpath, const struct stat *sb, int typeflag),
        int nopenfd);
*)

external ftw:
  string -> (string -> void ptr -> int -> int) funptr -> int -> int = "ftw"


let _FTW_F = [%c constant "FTW_F" camlint]
let _FTW_D = [%c constant "FTW_D" camlint]

let show path _ typ =
  let info =
    if typ = _FTW_F then
      "regular file"
    else if typ = _FTW_D then
      "directory"
    else
      "something else" in
  Printf.printf "%S (%s)\n" path info;
  0

let () =
  let path = match Sys.argv with
  | [| _ ; b |] -> b
  | _ -> "." in
  let res = ftw path show 32 in
  if res = 0 then
    exit 0
  else
    exit 1
