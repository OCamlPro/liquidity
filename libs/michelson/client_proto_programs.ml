
open Error_monad
open Hash
open Utils
open Tezos_data
open Tezos_context
open Michelson_parser
open Client_commands
open Script_located_ir

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
end

#include "../../tezos/src/client/embedded/alpha/client_proto_programs.ml"
