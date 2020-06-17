open Dune_Network_Lib
open LiquidTypes
open LiquidClientUtils

type const = LiquidTypes.syntax_const
type contract = LiquidTypes.syntax_contract
type datatype = LiquidTypes.datatype
type location = LiquidTypes.location
type loc_info = string option

type compiled_init =
  | No_init
  | Init_constant of const
  | Init_code of contract * (string * datatype) list

let unit = CUnit
let tuple l =  CTuple l
let print_loc = LiquidLoc.print_loc

let rec list_big_maps name acc storage storage_ty =
  match storage, storage_ty with
  | CBigMap BMId i, Tbigmap (k, v) ->
    let id = LiquidNumber.int_of_integer i in
    let id = match name with
      | Some name -> Bm_name (id, name)
      | None -> Bm_id id in
    (id, k, v) :: acc
  | ( CUnit
    | CBool _
    | CInt _
    | CNat _
    | CTez _
    | CTimestamp _
    | CString _
    | CBytes _
    | CKey _
    | CNone
    | CSignature _
    | CKey_hash _
    | CContract _
    | CLambda _), _ -> acc
  | CTuple l, Ttuple tys ->
    List.fold_left2 (list_big_maps name) acc l tys
  | CSome c, Toption ty
  | CLeft c, Tor (ty, _)
  | CRight c, Tor (_, ty) ->
    list_big_maps name acc c ty
  | CMap l, Tmap (tk, tv) ->
    List.fold_left (fun acc (k, v) ->
        let acc = list_big_maps name acc k tk in
        list_big_maps name acc v tv
      ) acc l
  | CList l, Tlist ty
  | CSet l, Tset ty ->
    List.fold_left (fun acc c -> list_big_maps name acc c ty) acc l
  | CRecord l, Trecord (_, tys) ->
    List.fold_left2 (fun acc (field, c) (_, ty) ->
        let name = match name with
          | None -> Some field
          | Some name -> Some (String.concat "." [name; field]) in
        list_big_maps name acc c ty
      ) acc l tys
  | CConstr (n, c), Tsum (_, tys) ->
    List.fold_left (fun acc (c_name, ty) ->
        if c_name <> n then acc
        else
          let name = match name with
            | None -> Some c_name
            | Some name -> Some (String.concat "." [name; c_name]) in
          list_big_maps name acc c ty
      ) acc tys
  | _, _ -> acc

let list_big_maps storage storage_ty =
  list_big_maps None [] storage storage_ty


let storage c = c.storage
let entries c =
  List.map (fun e ->
      e.entry_sig.entry_name, e.entry_sig.parameter
    ) c.entries


let rec apply_big_map_subst subst storage =
  let apply = apply_big_map_subst subst in
  match storage with
  | CBigMap BMId id ->
    CBigMap (BMList (List.assoc (LiquidNumber.int_of_integer id) subst))
  | ( CBigMap BMList _
    | CUnit
    | CBool _
    | CInt _
    | CNat _
    | CTez _
    | CTimestamp _
    | CString _
    | CBytes _
    | CKey _
    | CNone
    | CSignature _
    | CKey_hash _
    | CContract _
    | CLambda _) as c -> c
  | CTuple l -> CTuple (List.map apply l)
  | CSome c -> CSome (apply c)
  | CLeft c -> CLeft (apply c)
  | CRight c -> CRight (apply c)
  | CMap l -> CMap (List.map (fun (k, v) -> apply k, apply v) l)
  | CList l -> CList (List.map apply l)
  | CSet l -> CSet (List.map apply l)
  | CRecord l -> CRecord (List.map (fun (f, v) -> f, apply v) l)
  | CConstr (n, c) -> CConstr (n, apply c)

let default_empty_const ty = LiquidData.default_empty_untyped_const ty

(* Used for sharing metadata/environment with compilation/decompilation *)
let global_ty_env = ref (LiquidFromParsetree.initial_env "")
let global_contract_sig = ref LiquidTypes.dummy_contract_sig
let global_type_annots = ref (Hashtbl.create 0)
let global_types = ref []

let parse_const s =
  let env = !global_ty_env in
  s
  |> LiquidFromParsetree.expression_of_string
  |> LiquidFromParsetree.translate_expression env
  (* |> LiquidCheck.typecheck_code tenv *)
  |> LiquidData.translate_const_exp
(* |> LiquidCheck.typecheck_const tenv *)

let parse_contract s =
  let ocaml_asts = match s with
    | From_strings ss ->
      List.map (fun s ->
          "liquidity_buffer",
          LiquidFromParsetree.structure_of_string ~filename:"liquidity_buffer"
            s) ss
    | From_files files ->
      List.map (fun f -> f, LiquidFromParsetree.read_file f) files
  in
  let syntax_ast = LiquidFromParsetree.translate_multi ocaml_asts in
  syntax_ast
(* let typed_ast = LiquidCheck.typecheck_contract
 *     ~warnings:true ~decompiling:false syntax_ast in
 * global_ty_env := typed_ast.ty_env;
 * let contract_sig = LiquidTypes.full_sig_of_contract typed_ast in
 * global_contract_sig := contract_sig;
 * typed_ast *)

let parse_datatype s =
  LiquidFromParsetree.type_of_string s
  |> LiquidFromParsetree.translate_type !global_ty_env

let print_const c =
  let env = !global_ty_env in
  let tenv = empty_typecheck_env ~warnings:true
      LiquidTypes.dummy_contract_sig env in
  (* LiquidData.translate_const_exp const *)
  c
  |> LiquidCheck.typecheck_const tenv
  (* |> LiquidSimplify.simplify_const *)
  (* |> LiquidDecode.decode_const *)
  (* |> LiquidUntype.untype_const *)
  |> LiquidPrinter.Liquid.string_of_const

let print_contract c =
  let untyped_ast =
    LiquidCheck.typecheck_contract
      ~keep_tvars:true ~warnings:false ~decompiling:true c
  in
  From_strings [LiquidPrinter.Syntax.string_of_structure
                  (LiquidToParsetree.structure_of_contract
                     ~type_annots:!global_type_annots ~types:!global_types untyped_ast) []
               ]


let print_datatype ty =
  LiquidPrinter.Liquid.string_of_type ty

(** unsused **)
let (const_encoding : const Json_encoding.encoding) =
  Json_encoding.conv
    (fun _ -> failwith "Liquidity.const_encoding unimplemented")
    (fun _ -> failwith "Liquidity.const_encoding unimplemented")
    Json_encoding.unit

let (contract_encoding : contract Json_encoding.encoding)  =
  Json_encoding.conv
    (fun _ -> failwith "Liquidity.contract_encoding unimplemented")
    (fun _ -> failwith "Liquidity.contract_encoding unimplemented")
    Json_encoding.unit

let (datatype_encoding : datatype Json_encoding.encoding)  =
  Json_encoding.conv
    (fun _ -> failwith "Liquidity.datatype_encoding unimplemented")
    (fun _ -> failwith "Liquidity.datatype_encoding unimplemented")
    Json_encoding.unit


let const = new Lazy_superposed.superposer (object
  method parse = parse_const
  method print = print_const
  method encoding = const_encoding
end)

let contract = new Lazy_superposed.superposer (object
  method parse s = parse_contract (From_strings [s])
  method print c = match print_contract c with
    | From_strings [s] -> s
    | _ -> assert false
  method encoding = contract_encoding
end)

let datatype = new Lazy_superposed.superposer (object
  method parse = parse_datatype
  method print = print_datatype
  method encoding = datatype_encoding
end)
