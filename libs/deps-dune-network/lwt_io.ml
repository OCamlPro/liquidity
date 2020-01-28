(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)


type _ kind =
  Input : in_channel kind
| Output : out_channel kind

let with_file : type a. mode:a kind -> string ->
                             (a -> 'b) ->
                             'b
  = fun ~mode file f ->
  match mode, f with
  | Input, f ->
     let ic = open_in_bin file in
     let x = f ic in
     close_in ic;
     x
  | Output, f ->
     let oc = open_out_bin file in
     let x = f oc in
     close_out oc;
     x

let write oc str = output_string oc str; Lwt.return ()
let read ic =
  let s  = Bytes.create 64_000 in
  let len = input ic s 0 (Bytes.length s) in
  Lwt.return (Bytes.sub s 0 len)
