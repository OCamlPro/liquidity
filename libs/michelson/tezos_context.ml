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

module Script_prim = struct
  include Script_prim_repr
end

module Contract = struct
  include Contract_repr
  include Contract_storage
            (*
  let exists ( _ctxt : context ) (_contract : t) = ok (assert false : bool)
  let get_script (_ctxt : context) ( _contract : t) =
    (assert false : (Script.t option) tzresult Lwt.t )
  let get_manager (_ctxt : context) (_contract : t) =
    ok (assert false: public_key_hash)
  let spend_from_script (_ctxt : context) (_contract: t) (_:Tez.t) =
    ok (assert false : context)
  let  (assert false : t Data_encoding.t)encoding =
    Data_encoding.describe
                           ~
  let credit (_ctxt: context) (_contract: t) (_amount : Tez.t) =
    ok (assert false : context )
  let update_script_storage_and_fees
        ( _ctxt : context) (_contract: t)
    ( _fee : Tez.t) ( _storage : Script.expr) =
    ok (assert false : context)
  let originate
        (_ : context)
        (_ : origination_nonce)
        ~manager ~delegate ~balance
        ?script ~spendable ~delegatable =
    ok (assert false : context * t * origination_nonce)
  let get_balance (_ : context) (_: t) = ok (assert false : Tez.t)
             *)
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
