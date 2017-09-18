
open Hash

module Node_rpc_services : sig
  module Blocks : sig
    type block = [ `Prevalidation ]
  end
end

module Client_rpcs : sig

  type logger = unit

  type config = {
      host : string ;
      port : int ;
      tls : bool ;
      logger : logger ;
    }

  val default_config :config


end

#include "../../tezos/src/client/client_commands.mli"
