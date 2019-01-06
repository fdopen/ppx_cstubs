(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018 fdopen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>. *)

type id
type intern

type extract_info = {
  id : id;
  single_prog : string;
  intern : intern;
}

val prologue : string

(* buf can be used to extract several values
   from one object file. User must add prologue himself manually
   before the first call *)
val prepare_extract_int :
  ?disable_checks:bool ->
  ?min:string ->
  ?max:string ->
  buf:Buffer.t ->
  c_header:string -> expr:string -> signed:bool -> unit -> extract_info

val prepare_extract_string :
  buf:Buffer.t -> c_header:string -> expr:string -> unit -> extract_info

type obj

val compile : ?ebuf:Buffer.t -> string -> (obj, string) CCResult.t

type extract_error =
  | Info_not_found
  | Overflow of string
  | Underflow of string
  | User_overflow of string
  | User_underflow of string
  | Not_an_integer

val extract : extract_info -> obj -> (string, extract_error) CCResult.t
