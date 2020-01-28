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

let cut_at s seps =
  let pos = List.fold_left (fun pos c ->
      match pos with
      | Some _ -> pos
      | None -> String.index_opt s c
    ) None seps in
  match pos with
  | None -> s, None
  | Some pos ->
    String.sub s 0 pos,
    Some (String.sub s (pos+1) (String.length s - pos -1))

let of_string s =
  let date, hour = cut_at s ['T'; ' '] in
  let hour, timezone =
    match hour with
    | None -> "00:00:00", "Z"
    | Some s ->
      let hour, timezone = cut_at s ['+'] in
      let hour, timezone = match timezone with
        | None ->
          let hour, _ = cut_at s ['Z'] in
          hour, "Z"
        | Some timezone ->
          hour, "+" ^ timezone
      in
      let hour = if String.length hour = 5 then hour ^ ":00" else hour in
      hour, timezone
  in
  Printf.sprintf "%sT%s%s" date hour timezone
