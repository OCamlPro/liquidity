(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'a t = 'a

let return a = a
let bind x f = f x
let return_nil = return []
let return_none = return None
let catch t f =
  try t () with exn -> f exn

module Infix = struct

  let (>>=) x f  = f x
  let (>|=) x f =  f x

end
include Infix
