open Hash
open Tezos_context

module Node_rpc_services = struct
  module Blocks = struct
    type block = [ `Prevalidation ]
  end
end

module Client_rpcs = struct

  type logger = unit

  type config = {
      host : string ;
      port : int ;
      tls : bool ;
      logger : logger ;
    }

  let default_config = {
      host = "localhost" ;
      port = 8732 ;
      tls = false ;
      logger = () ;
    }


end

#include "../../tezos/src/client/client_commands.ml"

(*
type ('a, 'b) lwt_format =
  ('a, Format.formatter, unit, 'b Lwt.t) format4

type context = {
    warning : 'a. ('a, unit) lwt_format -> 'a ;
  }

type command = unit
 *)
