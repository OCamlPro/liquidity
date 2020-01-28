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

open Jsonm
(* String conversion *)
exception Escape of ((int * int) * (int * int)) * error

module TYPES = struct
  type t =
    Z
  | B of bool
  | F of float
  | S of string
  | L of t list
  | O of (string * t) list

end
open TYPES

let json_of_src ?encoding src =
  let dec d = match decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | `Null -> k Z d
    | `Bool b -> k (B b) d
    | `String s -> k (S s) d
    | `Float f -> k (F f) d
    | _ -> assert false
  and arr vs k d = match dec d with
    | `Ae -> k (L (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
    | `Oe -> k (O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)

let of_string str: t =
  match json_of_src (`String str) with
  | `JSON j  -> j
  | `Error _ -> failwith "json_of_string"

let rec print indent t =
  Printf.printf "%s" indent;
  begin
  match t with
    Z -> Printf.printf "Z"
  | B bool -> Printf.printf "B %b" bool
  | F float -> Printf.printf "F %f" float
  | S string -> Printf.printf "S %S" string
  | L  list ->
    Printf.printf "L [\n";
    List.iter (print (indent ^ "  ")) list;
    Printf.printf "%s  ]" indent;
  | O list ->
    Printf.printf "O [\n";
    let indent4 = indent ^ "    " in
    List.iter (fun (s, t) ->
      Printf.printf "%s  %S =\n" indent s;
      print indent4 t;
    ) list;
    Printf.printf "%s  ]" indent;
  end;
  Printf.printf "\n"

let print = print "  "

let to_dst ?(minify=true) dst json =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
  let rec value v k e = match v with
    | Z -> enc e `Null; k e
    | B b -> enc e (`Bool b); k e
    | F f -> enc e (`Float f); k e
    | S s  -> enc e (`String s); k e
    | L vs -> arr vs k e
    | O ms -> obj ms k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with
    | v :: vs' -> value v (arr_vs vs' k) e
    | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with
    | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
    | [] -> enc e `Oe; k e
  in
  let e = Jsonm.encoder ~minify dst in
  let finish e = ignore (Jsonm.encode e `End) in
  value json finish e

let to_buffer ?minify buf json =
  to_dst ?minify (`Buffer buf) json

let to_string ?minify json =
  let buf = Buffer.create 1024 in
  to_buffer ?minify buf json;
  Buffer.contents buf
