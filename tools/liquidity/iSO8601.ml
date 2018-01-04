(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let cut_at s c =
  try
    let pos = String.index s c in
    String.sub s 0 pos,
    Some (String.sub s (pos+1) (String.length s - pos -1))
  with Not_found -> s, None

let of_string s =
  let date, hour = cut_at s 'T' in
  let hour, timezone =
    match hour with
    | None -> "00:00:00", "Z"
    | Some s ->
      let hour, timezone = cut_at s '+' in
      let hour, timezone = match timezone with
        | None ->
          let hour, _ = cut_at s 'Z' in
          hour, "Z"
        | Some timezone ->
          hour, "+" ^ timezone
      in
      let hour = if String.length hour = 5 then hour ^ ":00" else hour in
      hour, timezone
  in
  Printf.sprintf "%sT%s%s" date hour timezone
