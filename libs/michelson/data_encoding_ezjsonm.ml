(*
#include "../../tezos/src/utils/data_encoding_ezjsonm.ml"
 *)
let to_string _json = assert false
let from_string _json = assert false
let read_file _filename = assert false
let write_file _filename _json = assert false

let to_root = function
  | `O ctns -> `O ctns
  | `A ctns -> `A ctns
  | `Null -> `O []
  | oth -> `A [ oth ]

let to_string j = Ezjsonm.to_string ~minify:true (to_root j)

let from_string s =
  try (Ezjsonm.from_string s :> Data_encoding.json)
  with Ezjsonm.Parse_error (_, msg) -> failwith ("error: " ^ msg)
