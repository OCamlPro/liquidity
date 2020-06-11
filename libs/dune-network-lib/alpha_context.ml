type t = unit

module type BASIC_DATA = sig
  type t
  include Compare.S with type t := t
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
end

module Tez = Tez_repr
module Period = Period_repr

module Timestamp = struct
  include Time_repr
end


module Love_repr = struct
  let is_code _ = assert false
  let is_const _ = assert false
end

(* module Script = struct *)
type dummy_expr =
  | Michelson_expr of unit
  | Dune_code of unit
  | Dune_expr of unit
(* end *)

open Love_pervasives
open Love_value

type dummy_code =
  | Contract of Love_ast.top_contract
  | LiveContract of LiveContract.t
  | FeeCode of FeeCode.t

type dummy_const =
  | Value of Value.t
  | Type of Love_type.t

module Script_all = struct
  type dummy_expr =
    | Michelson_expr of unit
    | Dune_code of unit
    | Dune_expr of unit

  (* include Love_script_repr *)
  let force_decode _ _ = assert false
end

module Script_int = Script_int_repr

module Script_timestamp = struct
  include Script_timestamp_repr
  let now _ = assert false
end
type dummy_gas =
  | Unaccounted
  | Limited of { remaining : Z.t }

module Gas = struct
  include Gas_limit_repr
  type error += Gas_limit_too_high (* = Raw_context.Gas_limit_too_high *)
  let consume _ _ = ok ()
  let level _ = assert false
end
module Cycle = struct
  let to_int32 _ = assert false
end
type dummy_level = { cycle : int; level : int }
module Level = struct
  let current _ = assert false
end
module Raw_level = struct
  let to_int32 _ = assert false
end
module Contract = struct
    include Contract_repr
    let exists _ _ = return false
    let get_script _ _ = assert false
    let fresh_contract_from_current_nonce _ = assert false
    let get_balance _ _= assert false
    let get_delegation _ _= assert false
    let get_admin _ _= assert false
    let get_whitelist _ _= assert false
  end
module Big_map = struct
  type id = Z.t
  let fresh _ = assert false
  let fresh_temporary _ = assert false
  let mem _ _ _ = assert false
  let get_opt _ _ _ = assert false
  let cleanup_temporary _ = assert false
  let exists _ _ = assert false
end
module Roll = struct
  module Delegate = struct
    let get_maxrolls _ _ = assert false
  end
end

let fresh_internal_nonce _ = ok ((), 0)
