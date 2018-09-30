(* module S = struct
 *   #include "../../tezos/src/lib_crypto/s.ml"
 * end *)

module Clic = struct
  let param ~name ~desc _ _ = ()
  let parameter _ = ()
end

module RPC_arg = struct
  type 'a t
  let make
      ~name
      ~descr
      ~destruct
      ~construct
      () = () (* assert false *)
end

module Helpers = struct
  #include "../../tezos/src/lib_crypto/helpers.ml"
end

#include "../../tezos/src/lib_crypto/blake2B.ml"
