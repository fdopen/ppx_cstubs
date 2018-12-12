let%c () = header {|
#include <time.h>
#include <errno.h>
|}

(* Note: OCaml 4.02.3 doesn't understand

  type%c x = ...
  external%c x : ....

  Only the following syntax is supported by OCaml 4.02.3 as well:
  [%%c type x = ... ]
  [%%c external x : ... ]

*)
type%c tm = {
  tm_sec: int;
  tm_min: int;
  tm_hour: int;
  tm_mday: int;
  tm_mon: int;
  tm_year: int;
  tm_wday: int;
  tm_yday: int;
  tm_isdst: int } [@@ as_record]

open Ctypes
open PosixTypes

(* PosixTypes not open in the declarations below :D *)
external time : PosixTypes.time_t ptr -> PosixTypes.time_t = "time"
external asctime: tm ptr -> string_opt = "asctime" [@@ return_errno]
external localtime: PosixTypes.time_t ptr -> tm ptr_opt = "localtime"

let _EOVERFLOW = [%c constant "EOVERFLOW" sint]

let () =
  let timep = allocate_n ~count:1 time_t in
  let time = time timep in
  assert (time = !@timep);
  let ptm = match localtime timep with
  | None -> prerr_endline "localtime failure"; exit 1
  | Some s -> s in
  let tm = !@ptm in
  Printf.printf "tm.tm_mon  = %d\n" tm.tm_mon;
  Printf.printf "tm.tm_year = %d\n" tm.tm_year;
  let strt,errno = asctime ptm in
  match strt with
  | Some s -> print_endline s
  | None ->
    if errno = _EOVERFLOW then
      prerr_endline "overflow"
    else
      Printf.eprintf "unknown error:%s\n" (Signed.SInt.to_string errno);
    exit 1
