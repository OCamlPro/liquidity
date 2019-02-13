(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let milion = Z.of_int 1_000_000

let mic_mutez_of_tez { tezzies ; mutez } =
  let extra_mutez = match mutez with
    | None -> Z.zero
    | Some mutez -> Z.of_string mutez in
  Z.of_string tezzies
  |> Z.mul milion
  |> Z.add extra_mutez

let mic_of_integer { integer } = integer

let int_of_integer { integer } = Z.to_int integer
let integer_of_int int =
  let integer = Z.of_int int in
  { integer }

let tez_of_mic_mutez z =
  let z_tezzies, z_mutez = Z.div_rem z milion in
  let tezzies = Z.to_string z_tezzies in
  let mutez =
    if Z.equal z_mutez Z.zero then None else Some (Z.to_string z_mutez) in
  { tezzies; mutez }

let integer_of_mic integer = { integer }

let remove_underscores s =
  let b = Buffer.create 10 in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
    | '_' -> ()
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let integer_of_liq s =
  let integer = remove_underscores s |> Z.of_string in
  { integer }

(* TODO: beware of overflow... *)
let tez_of_liq s =
  let s = remove_underscores s in
  try
    let pos = String.index s '.' in
    let len = String.length s in
    let tezzies = String.sub s 0 pos in
    let mutez = String.sub s (pos+1) (len - pos - 1) in
    let mutez_len = String.length mutez in
    let mutez = match mutez_len with
      | 0 -> None
      | l when l <= 6 ->
        let mutez = String.init 6 (fun i ->
            if i < l then mutez.[i] else '0'
          ) in
        Some mutez
      | _ -> invalid_arg "bad mutez in tez_of_liq"
    in
    { tezzies; mutez }
  with Not_found ->
    { tezzies = s; mutez = None }

let liq_of_tez { tezzies ; mutez } =
  match mutez with
  | None -> tezzies
  | Some mutez ->
    let mutez = Printf.sprintf "%06d" (int_of_string mutez) in
    let len = ref 0 in
    for i = String.length mutez - 1 downto 0 do
      if !len = 0 && mutez.[i] <> '0' then len := i + 1
    done;
    let mutez = String.sub mutez 0 !len in
    String.concat "." [tezzies; mutez]

let liq_of_integer { integer } = Z.to_string integer
