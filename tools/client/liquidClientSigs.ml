
module Liquidity = LiquidityLang

module type LANG = sig

  (* module Source : sig
   *   type const
   *   (\* type expr *\)
   *   type contract
   *   type datatype
   *   type loc_info
   *   type location
   *
   *   val unit : const
   *   val tuple : const list -> const
   *   val list_big_maps : const -> datatype -> (bm_id * datatype * datatype) list
   *   (\* val string_of_const : const -> string *\)
   *   val storage : contract -> datatype
   *   val entries : contract -> (string * datatype) list
   *   val apply_big_map_subst : (int * (const * const) list) list -> const -> const
   *   val default_empty_const : datatype -> const
   *
   *   val print_loc : Format.formatter -> location -> unit
   *
   *   val parse_contract : from -> contract
   *
   *   val datatype : datatype Lazy_superposed.superposer
   *   val const : const Lazy_superposed.superposer
   *   val contract : contract Lazy_superposed.superposer
   *
   *   (\**/* unsused **\)
   *   val const_encoding : const Json_encoding.encoding
   *   val contract_encoding : contract Json_encoding.encoding
   *   val datatype_encoding : datatype Json_encoding.encoding
   *
   * end *)

  module Target : sig
    type const
    (* type expr *)
    type contract
    type location
    type datatype

    val name : string
    val unit : const
    val compare_loc : location -> location -> int
    val next_loc : location -> location
    val loc_encoding : location Json_encoding.encoding

    val const_encoding : const Json_encoding.encoding
    val contract_encoding : contract Json_encoding.encoding
    val datatype_encoding : datatype Json_encoding.encoding

    val datatype : datatype Lazy_superposed.superposer
    val const : const Lazy_superposed.superposer
    val contract : contract Lazy_superposed.superposer

  end

  val compile_contract :
    Liquidity.contract ->
    Target.contract * Liquidity.compiled_init *
    (Target.location * (Liquidity.location * Liquidity.loc_info)) list

  val decompile_contract : Target.contract -> Liquidity.contract

  val compile_const : ?ty:Liquidity.datatype -> Liquidity.const -> Target.const

  val decompile_const : ?ty:Liquidity.datatype -> Target.const -> Liquidity.const

  val compile_datatype : Liquidity.datatype -> Target.datatype

end
