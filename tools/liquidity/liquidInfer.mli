(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val mk_tvar : string -> datatype
val fresh_tvar : unit -> datatype
val wrap_tvar : datatype -> datatype
val has_tvar : datatype -> bool
val make_subst : string list -> datatype list -> (string * datatype) list

val instantiate_to : (string * datatype) list -> datatype -> datatype
val instantiate : StringSet.t * datatype -> datatype

val unify : location -> datatype -> datatype -> unit
val generalize : datatype -> datatype -> unit

val find_variant_type :
  loc:location -> env ->
  (pattern * 'a) list -> datatype option

val make_type_eqn :
  location -> (datatype list * datatype) list -> datatype list -> datatype

val mono_contract : typed_contract -> typed_contract
