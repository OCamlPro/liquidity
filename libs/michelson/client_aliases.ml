open Tezos_context

module Lwt_utils = struct
  let create_dir dirname = assert false

end

#include "../../tezos/src/client/client_aliases.ml"

(*
module type Entity = sig type t end
module type Alias = sig type t end

module Alias(S : Entity) = struct type t = S.t end
 *)
