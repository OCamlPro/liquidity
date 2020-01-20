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

open LiquidTypes

type from =
  | From_strings of string list
  | From_files of string list

let post = ref (fun ~data _ ->
    failwith "mini version cannot do post request")

let get = ref (fun _ ->
    failwith "mini version cannot do get request")

type key_diff =
  | DiffKeyHash of string
  | DiffKey of typed_const

type big_map_diff_item =
  | Big_map_add of key_diff * typed_const
  | Big_map_remove of key_diff

type big_map_diff = big_map_diff_item list

type stack_item =
  | StackConst of typed_const
  | StackCode of int

type trace_item = {
  loc : location option;
  gas : int;
  stack : (stack_item * string option) list;
}

type trace = trace_item array

type internal_operation =
  | Reveal of string
  | Transaction of {
      amount : string;
      destination : string;
      parameters : typed_const option;
    }
  | Origination of {
      delegate: string option ;
      script: (typed_contract * typed_const) option ;
      balance: string ;
    }
  | Delegation of string option

type operation = {
  source : string;
  nonce : int;
  op : internal_operation;
}

exception RequestError of int * string
exception ResponseError of string
exception RuntimeError of error * trace option
exception LocalizedError of error
exception RuntimeFailure of error * string option * trace option



module type S = sig
  type 'a t
  val run : from -> string -> string -> string ->
    (operation list * LiquidTypes.typed_const * big_map_diff option) t
  val run_debug : from -> string -> string -> string ->
    (operation list * LiquidTypes.typed_const * big_map_diff option * trace) t
  val init_storage : from -> string list -> LiquidTypes.encoded_const t
  val forge_deploy_script :
    source:string -> from -> string list ->
    (string * string * LiquidToMicheline.loc_table) t
  val forge_deploy : from -> string list -> string t
  val deploy : from -> string list -> (string * (string, exn) result) t
  val get_storage : from -> string -> LiquidTypes.typed_const t
  val get_big_map_value :
    from -> string -> string -> LiquidTypes.typed_const option t
  val forge_call_parameter :
    from -> string -> string -> string * LiquidToMicheline.loc_table
  val forge_call : from -> string -> string -> string -> string t
  val call : from -> string -> string -> string ->
    (string * (unit, exn) result) t
  val activate : secret:string -> string t
  val inject : operation:string -> signature:string -> string t
  val pack : ?liquid:from -> const:string -> ty:string -> string t
end

module Dummy = struct

  let run _ _ _ _ =
    failwith "mini version cannot run"

  let run_debug _ _ _ _ =
    failwith "mini version cannot run debug"

  let init_storage _ _ =
    failwith "mini version cannot deploy"

  let forge_deploy_script ~source:_ _ _ =
    failwith "mini version cannot deploy"

  let forge_deploy _ _ =
    failwith "mini version cannot deploy"

  let deploy _ _ =
    failwith "mini version cannot deploy"

  let get_storage _ _ =
    failwith "mini version cannot query node"

  let get_big_map_value _ _ _ =
    failwith "mini version cannot query node"

  let forge_call_parameter _ _ _ =
    failwith "mini version cannot call"

  let forge_call _ _ _ _ =
    failwith "mini version cannot call"

  let call _ _ _ _ =
    failwith "mini version cannot call"

  let activate ~secret =
    failwith "mini version cannot activate"

  let inject ~operation ~signature =
    failwith "mini version cannot inject"

  let pack ?liquid ~const ~ty =
    failwith "mini version cannot pack"
end

module Async = struct include Dummy type 'a t = 'a Lwt.t end

module Sync = struct include Dummy type 'a t = 'a end

let forge_call_arg ?entry_name liquid arg = ""
