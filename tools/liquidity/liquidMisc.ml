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
