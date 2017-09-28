(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Lwt = Lwt
include Error_monad

type t = Storage.t
(*  {
    warning : 'a. ('a, Format.formatter, unit) format -> 'a;
  } *)
type context = t
type public_key = Ed25519.Public_key.t
type public_key_hash = Ed25519.Public_key_hash.t
type signature = Ed25519.Signature.t


module Script_int = struct
  include Script_int_repr
end

module Tez = struct
  include Tez_repr
end

let current_time = ref (Int64.of_float (Unix.gettimeofday ()))

module Timestamp = struct
  type t = int64
  let compare = Int64.compare
  let to_notation = Int64.to_string
  let of_notation s = Some (Int64.of_string s)
  (* TODO: what's the expected format of seconds ? Hre, we assume integers *)
  let of_seconds s = try Some (Int64.of_string s) with _ -> None
  let current _ctxt = !current_time
  let (+?) x y = ok (Int64.add x y)
  let (-?) x y = ok (Int64.sub x y)
  let ( *?) x y = ok (Int64.mul x y)

end

module Script = struct
  include Script_repr
end

module Contract = struct
  include Contract_repr
  include Contract_storage
end



module Ed25519 = struct
  include Ed25519
end

module Period = struct
  include Period_repr
end

(* TODO : catch overflows
 *)

module Constants = struct
  (*  let origination_burn = 1_00L (* TODO *) *)
  include Constants_repr
end

module Public_key = struct
  let get _ctxt _key = ok ( assert false : signature) (* TODO: should be a key ??? *)
end

module Voting_period = Voting_period_repr

module Local_environment = struct
  module Environment = struct
    module Error_monad = Error_monad
  end
end
