
open Hash
open Tezos_context

type error += Wrong_voting_period of Voting_period.t * Voting_period.t (* `Temporary *)
type error += Wrong_endorsement_predecessor of Block_hash.t * Block_hash.t (* `Temporary *)
type error += Bad_contract_parameter of Contract.t * Script.expr option * Script.expr option (* `Permanent *)
