(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val fresh_tvar : unit -> datatype
val free_tvars : datatype -> StringSet.t
val has_tvar : datatype -> bool

val instantiate_to : (string * datatype) list -> datatype -> datatype
val instantiate : StringSet.t * datatype -> datatype

val unify : location -> datatype -> datatype -> unit

val find_variant_type : typecheck_env -> (pattern * 'a) list -> datatype option

val make_type_eqn :
  location -> (datatype list * datatype) list -> datatype list -> datatype

val mono_contract :
  typecheck_env -> location -> typed_contract -> typed_contract
