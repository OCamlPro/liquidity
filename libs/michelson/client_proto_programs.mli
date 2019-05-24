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

open Error_monad
open Hash
open Utils
open Tezos_data
open Tezos_context

#include "../../tezos/src/client/embedded/alpha/client_proto_programs.mli"

         (*
open Tezos_context
open Error_monad

val parse_program: string -> Script.code tzresult Lwt.t
val parse_data: string -> Script.expr tzresult Lwt.t
val parse_data_type: string -> Script.expr tzresult Lwt.t
          *)

val print_program :
  (Tezos_context.Script.location -> int option) ->
  Format.formatter ->
  Tezos_context.Script.code *
    (Tezos_context.Script.location *
       (Tezos_context.Script.expr list * Tezos_context.Script.expr list))
  list -> unit

            (*
val no_locations : 'a -> 'b option
          *)
val print_expr :
  (int -> int option) -> Format.formatter -> Script.expr -> unit

                                                              (*
val unexpand_macros :
           (int * ('a * 'b)) list ->
           Script.code -> (int * ('a * 'b)) list * Script.code
          *)
