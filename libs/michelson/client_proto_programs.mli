
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
