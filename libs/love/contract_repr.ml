
#include "../../dune-network/src/proto_005_PsBabyM1/lib_protocol/contract_repr.ml"

let mk_originated_contract id = Originated id

let set_origination_nonce_index nonce index =
  let origination_index = Int32.of_int index in
  { nonce with origination_index }
