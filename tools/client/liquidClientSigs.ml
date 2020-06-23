
type bm_id =
  | Bm_id of int
  | Bm_name of int * string

module type LANG = sig

  module Source : sig
    type const
    (* type expr *)
    type contract
    type datatype
    type loc_info
    type location

    val unit : const
    val tuple : const list -> const
    val list_big_maps : const -> datatype -> (bm_id * datatype * datatype) list
    (* val string_of_const : const -> string *)
    val storage : contract -> datatype
    val entries : contract -> (string * datatype) list
    val apply_big_map_subst : (int * (const * const) list) list -> const -> const
    val default_empty_const : datatype -> const

    val print_loc : Format.formatter -> location -> unit

    (**/* unsused **)
    val const_encoding : const Json_encoding.encoding
    val contract_encoding : contract Json_encoding.encoding

  end

  module Target : sig
    type const
    (* type expr *)
    type contract
    type location
    type datatype

    val unit : const
    val compare_loc : location -> location -> int
    val next_loc : location -> location
    val loc_encoding : location Json_encoding.encoding

    val const_encoding : const Json_encoding.encoding
    val contract_encoding : contract Json_encoding.encoding
    val datatype_encoding : datatype Json_encoding.encoding

  end

  type compiled_init =
    | No_init
    | Init_constant of Source.const
    | Init_code of Source.contract * (string * Source.datatype) list

  val compile_contract :
    Source.contract ->
    Target.contract * compiled_init *
    (Target.location * (Source.location * Source.loc_info)) list

  val decompile_contract : Target.contract -> Source.contract

  val compile_const : ?ty:Source.datatype -> Source.const -> Target.const

  val decompile_const : ?ty:Source.datatype -> Target.const -> Source.const

  val compile_datatype : Source.datatype -> Target.datatype

end


module type CONV = sig
  type from_datatype
  type from_const
  type from_contract
  type dest_datatype
  type dest_const
  type dest_contract
  val parse_const : from_const -> dest_const
  val parse_contract : from_contract -> dest_contract
  val parse_datatype : from_datatype -> dest_datatype
  val print_const : dest_const -> from_const
  val print_contract : dest_contract -> from_contract
  val print_datatype : dest_datatype -> from_datatype
end

module type TT = sig
  type datatype
  type const
  type contract
end

module type CONVERTER = sig
  (* module Source : TT
   * module Target : TT
   * module SourceFrom : TT
   * module TargetFrom : TT *)
  type source_from_datatype
  type source_from_const
  type source_from_contract
  type source_dest_datatype
  type source_dest_const
  type source_dest_contract
  type target_from_datatype
  type target_from_const
  type target_from_contract
  type target_dest_datatype
  type target_dest_const
  type target_dest_contract
  module SourceConv :
    CONV with type dest_const := source_dest_const
          and type dest_contract := source_dest_contract
          and type dest_datatype := source_dest_datatype
          and type from_const := source_from_const
          and type from_contract := source_from_contract
          and type from_datatype := source_from_datatype
  module TargetConv :
    CONV with type dest_const := target_dest_const
          and type dest_contract := target_dest_contract
          and type dest_datatype := target_dest_datatype
          and type from_const := target_from_const
          and type from_contract := target_from_contract
          and type from_datatype := target_from_datatype
end
