(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let list_init n f =
  let rec list_init i n f =
    if i = n then [] else
      (f i) :: (list_init (i+1) n f)
  in
  list_init 0 n f

let list_make n x =
  list_init n (fun _ -> x)

let rec list_remove n list =
  if n > 0 then
    match list with
    | [] -> failwith "list_remove"
    | _ :: tail ->
       list_remove (n-1) tail
  else list

let string_replace s c1 c2 =
  let rec look acc i =
    try
      let index = String.index_from s i c1 in
      look (index :: acc) (index + 1)
    with Not_found | Invalid_argument _ -> List.rev acc
  in
  let indexes = look [] 0 in
  if indexes = [] then s
  else
    let b = Bytes.of_string s in
    List.iter (fun i -> Bytes.set b i c2) indexes;
    Bytes.to_string b
