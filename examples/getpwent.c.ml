let%c () = header {|
#include <sys/types.h>
#include <pwd.h>
#include <errno.h>
|}

open Ctypes

(*
type declaration in C:
struct passwd {
  char   *pw_name;       /* username */
  char   *pw_passwd;     /* user password */
  uid_t   pw_uid;        /* user ID */
  gid_t   pw_gid;        /* group ID */
  char   *pw_gecos;      /* user information */
  char   *pw_dir;        /* home directory */
  char   *pw_shell;      /* shell program */
};
*)

type%c passwd = {
  pw_name: string;
  (* note: `string` works, because the fields are only set, but never read
     by c code in the following expamle. If you would create such a value
     from OCaml and pass it to C, you have to ensure manually, that the garbage
     collector doesn't free the memory referenced, eg. from
     pw_name. That's not possible, if you use `string` instead of
     `char ptr`. *)
  pw_passwd: string;
  pw_uid: uint32_t;
  pw_gid: uint32_t;
  pw_gecos: string;
  pw_dir: string;
  pw_shell: string } [@@ as_record]

external getpwent : void -> passwd ptr_opt = "getpwent"
external setpwent : void -> void = "setpwent"
external endpwent : void -> void = "endpwent"

let print_entry r =
  Printf.printf
    "name:%S, dir:%S, uid:%s, git:%s, shell:%S\n"
    r.pw_name
    r.pw_dir
    (Unsigned.UInt32.to_string r.pw_uid)
    (Unsigned.UInt32.to_string r.pw_gid)
    r.pw_shell

let demo_getpwent () =
  let rec iter () =
    match getpwent () with
    | None -> ()
    | Some r ->
      print_entry (!@ r);
      iter () in
  setpwent ();
  iter ();
  endpwent ()

let%c size_t' =
  view size_t ~read:Unsigned.Size_t.to_int ~write:Unsigned.Size_t.of_int

(* c prototype:
   int getpwent_r(struct passwd *pwbuf, char *buf,
                  size_t buflen, struct passwd **pwbufp);
*)
external getpwent_r :
  passwd ptr -> char ptr -> size_t' -> passwd ptr ptr -> int = "getpwent_r"
  [@@ release_runtime_lock]

let _ENOENT = [%c constant "ENOENT" camlint]

let demo_getpwent_r () =
  let pptr = allocate_n (ptr passwd) ~count:1 in
  let ptr = allocate_n passwd ~count:1 in
  let len = 8192 in
  let buf = CArray.make char len |> CArray.start in
  let rec iter () =
    let r = getpwent_r ptr buf len pptr in
    if r = _ENOENT then
      ()
    else if r <> 0 then
      let () = Printf.eprintf "unexpected errno:%d\n" r in
      exit 1
    else
    let r = !@ pptr in
    if Ctypes.is_null r then
      let () = prerr_endline "misbehaving getpwent_r implementation" in
      exit 1
    else
      print_entry ( !@ r );
    iter () in
  setpwent ();
  iter ();
  endpwent ()


let () =
  print_endline "getpwent:";
  demo_getpwent ();
  print_endline "\ngetpwent_r:";
  demo_getpwent_r ()
