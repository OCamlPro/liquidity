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
let return_unit = return ()
let catch t f =
  try t () with exn -> f exn
                         (*
type thread = Thread
type 'a u = Wakener
let wait _ = Thread, Wakener
let task () = (), Wakener
let wakeup _ _val = ()
let wakeup_exn _ _exn = ()
let async_exception_hook = ref (fun (exn : exn) -> ())
let async f = ()
                          *)
let fail s = raise s
module Infix = struct

  let (>>=) x f  = f x
  let (>|=) x f =  f x

end
include Infix

let finalize f g =
  match f () with
  | exception exn -> g (); raise exn
  | x -> g () ; x
