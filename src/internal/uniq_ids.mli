(* This file is part of ppx_cstubs (https://github.com/fdopen/ppx_cstubs)
 * Copyright (c) 2018-2019 fdopen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

val safe_ascii : char -> bool

val safe_ascii_only : string -> string

val safe_ascii_only_ml : string -> string

val unsuffixed_file_name : unit -> string

val safe_cname : prefix:string -> string

val safe_mlname : ?capitalize:bool -> ?prefix:string -> unit -> string

type merlin_state

val merlin_save : unit -> merlin_state

val merlin_restore : merlin_state -> unit
