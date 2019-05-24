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
open Michelson_parser
open Client_commands
open Script_located_ir
open Script

(* from Updater *)
type Error_monad.error += Ecoproto_error of Tezos_context.error (* Proto.error *) list


module Client_proto_rpcs = struct
  module Helpers = struct
    let typecheck_data _ = assert false
    let typecheck_code _ = assert false
    let run_code _ = assert false
    let trace_code _ = assert false
    let hash_data _ = assert false
  end
end

module Client_keys = struct
 module Secret_key = Client_aliases.Alias (struct
    type t = Ed25519.Secret_key.t
    let encoding = Ed25519.Secret_key.encoding
    let of_source _ s = Lwt.return (Ed25519.Secret_key.of_b58check s)
    let to_source _ p = return (Ed25519.Secret_key.to_b58check p)
    let name = "secret key"
  end)
end

module Environment = struct
  module Ed25519 = Ed25519
  module Error_monad = Error_monad
end

#include "../../tezos/src/client/embedded/alpha/client_proto_programs.ml"
