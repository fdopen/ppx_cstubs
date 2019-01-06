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

type io_out =
  [ `Buffer of Buffer.t
  | `Fd of Unix.file_descr
  | `Fun of string -> unit
  | `Null
  | `Stderr
  | `Stdout ]

type io_in =
  [ `Fd of Unix.file_descr
  | `Null
  | `String of string ]

val run :
     ?env:string array
  -> ?stdin:io_in
  -> ?stderr:io_out
  -> ?stdout:io_out
  -> string
  -> string list
  -> int

val cmd_to_string : string -> string list -> string
