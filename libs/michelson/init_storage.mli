(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val initialize : Storage.t -> Storage.t Error_monad.tzresult Lwt.t

val may_initialize :
  Context.t Lwt.t ->
  level:int32 ->
  timestamp:Int64.t ->
  fitness:MBytes.t list -> Storage.t Error_monad.tzresult Lwt.t
