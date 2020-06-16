type t = unit
type context = t

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

type dummy_code =
  | Contract of Love_ast_types.AST.top_contract
  | LiveContract of Love_value.LiveContract.t
  | FeeCode of Love_value.FeeCode.t

type dummy_const =
  | Value of Love_value.Value.t
  | Type of Love_ast_types.TYPE.t

module Dune_script_repr = struct
  type const = Const of dummy_const
  type code = Code of dummy_code
end

open Love_pervasives
open Love_value

module Script = struct

  type dummy_expr =
    | Michelson_expr of Script_repr.expr
    | Dune_code of Dune_script_repr.code
    | Dune_expr of Dune_script_repr.const

  type lazy_expr = unit

  type t = {
    code : lazy_expr ;
    storage : lazy_expr
  }

  type script_or_hash =
    | Script of t
    | Script_hash of {
        code_hash : Script_expr_hash.t ;
        storage : lazy_expr ;
      }
    | Script_code_hash of {
        code : lazy_expr ;
        code_hash : Script_expr_hash.t ;
        storage : lazy_expr ;
      }

  (* include Love_script_repr *)
  let force_decode _ _ = assert false
  let lazy_expr _ = assert false
end

type dummy_expr = Script.dummy_expr =
  | Michelson_expr of Script_repr.expr
  | Dune_code of Dune_script_repr.code
  | Dune_expr of Dune_script_repr.const

module Script_ir_translator = struct
  type dummy_ex_ty = Ex_ty of unit
  let parse_toplevel ~legacy:_ _ = assert false
  let parse_ty _ ~legacy:_
      ~allow_big_map:_ ~allow_operation:_
      ~allow_contract:_ _ = assert false
  let find_entrypoint ~root_name:_ _ _ = assert false
  let unparse_ty _ _ = assert false
end
type dummy_ex_ty = Script_ir_translator.dummy_ex_ty = Ex_ty of unit

module Love_michelson = struct
  let from_michelson_node_const _ _ = assert false
end

type packed_internal_operation = unit

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
    let get_script_code _ _ = assert false
    let get_storage _ _ = assert false
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
