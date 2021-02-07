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

open Std.Result

let real_init top = Main.init top

include Ppxc__script_real

let _init top =
  let top =
    match top with
    | Some t -> t
    | None ->
      object
        method eval expr =
          let p1 = Merlin_state.to_child () in
          let s = Marshal.to_string (p1, expr) [] in
          let b = Buffer.create 8192 in
          let prog = if Sys.win32 then "ppx_cstubs.exe" else "ppx_cstubs" in
          let args = [ "--run-merlin-top" ] in
          let eb = Buffer.create 128 in
          let ec =
            match
              Run.run ~stdin:(`String s) ~stdout:(`Buffer b)
                ~stderr:(`Buffer eb) prog args
            with
            | exception (Unix.Unix_error _ as e) ->
              Std.Util.error "failed to call ppx_cstubs: %s\n"
                (Printexc.to_string e)
            | x -> x
          in
          if ec <> 0 then
            Buffer.contents eb
            |> Std.Util.error "ipc error: child exit with %d (%S)" ec;
          let s = Buffer.contents b in
          match Marshal.from_string s 0 with
          | exception ((Failure _ | Invalid_argument _) as e) ->
            Std.Util.error "ipc error: marshaling from ppx_cstubs failed: %s"
              (Printexc.to_string e)
          | Ok s -> Merlin_state.from_child s
          | Error e -> Merlin_state.raise_error e

        method init ~nopervasives:_ ~pkgs:_ ~use_threads:_ ~cma_files:_ () = ()

        method is_merlin_ppx = true
      end
  in
  real_init top
