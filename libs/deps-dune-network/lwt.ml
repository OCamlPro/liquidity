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
