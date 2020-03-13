open LiquidClientSigs
open LiquidTypes
open Dune_Network_Lib

type const = LiquidTypes.syntax_const
type contract = LiquidTypes.syntax_contract
type datatype = LiquidTypes.datatype
type location = LiquidTypes.location
type loc_info = string option

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

(**/* unsused **)
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
