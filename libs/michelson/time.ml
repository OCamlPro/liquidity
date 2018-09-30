module RPC_arg = struct
  type 'a t
  let make
      ~name
      ~descr
      ~destruct
      ~construct
      () = assert false
end

#include "../../tezos/src/lib_base/time.ml"
