(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO: We could use a simplify pass to propagate the no-effect
  defining expression of once-used variables to their uniq use
  site. It could dramatically decrease the size of the stack.  *)


open LiquidTypes


let rec default_const = function
  | Tunit -> CUnit
  | Tbool -> CBool false
  | Tint -> CInt (LiquidPrinter.integer_of_int 0)
  | Tnat -> CNat (LiquidPrinter.integer_of_int 0)
  | Ttez -> CTez (LiquidPrinter.tez_of_liq "0")
  | Tstring -> CString ""
  | Tbytes -> CBytes "0x"
  | Ttimestamp -> CTimestamp "1970-01-01T00:00:00Z"
  | Tkey -> CKey "edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN"
  | Tkey_hash -> CKey_hash "tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc"
  | Tsignature ->
    CSignature
      "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk\
       68YpuGDeViW8wSXMr Ci5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7"
  | Tcontract _ -> CContract "KT1GE2AZhazRxGsAjRVkQccHcB2pvANXQWd7"
  | Taddress -> CAddress "KT1GE2AZhazRxGsAjRVkQccHcB2pvANXQWd7"
  | Ttuple l ->
    CTuple (List.map default_const l)
  | Toption ty -> CSome (default_const ty)
  | Tlist ty -> CList [default_const ty]
  | Tset ty -> CSet [default_const ty]
  | Tmap (ty1, ty2) ->
    CMap [default_const ty1, default_const ty2]
  | Tbigmap  (ty1, ty2) ->
    CBigMap [default_const ty1, default_const ty2]
  | Tor (ty, _) -> CLeft (default_const ty)
  | Trecord (_, fields) ->
    CRecord (
      List.map (fun (name, ty) ->
          name, default_const ty) fields
    )
  | Tsum (_, (c, ty) :: _) ->
    CConstr (c, default_const ty)

  | Tsum (_, [])
  | Tfail
  | Tclosure _
  | Tlambda _
  | Toperation -> raise Not_found

let rec translate_const_exp loc (exp : encoded_exp) =
  match exp.desc with
  | Let (_, loc, _, _) ->
     LiquidLoc.raise_error ~loc "'let' forbidden in constant"
  | Const (_loc, ty, c) -> c

  (* removed during typechecking *)
  | Record (_, _)
  | Constructor (_, _, _) -> assert false

  | Apply (Prim_Left, _, [x; _ty]) -> CLeft (translate_const_exp loc x)
  | Apply (Prim_Right, _, [x; _ty]) -> CRight (translate_const_exp loc x)
  | Apply (Prim_Some, _, [x]) -> CSome (translate_const_exp loc x)
  | Apply (Prim_Cons, _, list) ->
     CList (List.map (translate_const_exp loc) list)
  | Apply (Prim_tuple, _, list) ->
     CTuple (List.map (translate_const_exp loc) list)


  | Apply (prim, _, args)
    -> LiquidLoc.raise_error "<apply %s(%d) not yet implemented>"
                             (LiquidTypes.string_of_primitive prim)
                             (List.length args)
  | Var (_, _, _)
  | SetVar (_, _, _, _)
  | If (_, _, _)
  | Seq (_, _)
  | Transfer (_, _, _, _)
  | MatchOption (_, _, _, _, _)
  | MatchNat (_, _, _, _, _, _)
  | MatchList (_, _, _, _, _, _)
  | Loop (_, _, _, _)
  | Fold (_, _, _, _, _, _)
  | Map (_, _, _, _, _)
  | MapFold (_, _, _, _, _, _)
  | Lambda (_, _, _, _, _)
  | Closure (_, _, _, _, _, _)
  | MatchVariant (_, _, _)
  | Failwith (_, _)
  | CreateContract (_, _, _)
  | ContractAt (_, _, _)
    ->
    LiquidLoc.raise_error ~loc "non-constant expression"


let translate env contract_sig s ty =
  let ml_exp =
    LiquidFromOCaml.expression_of_string ~filename:env.filename s in
  (* hackish: add type annotation for constants *)
  let ml_ty = LiquidToOCaml.convert_type ~abbrev:false ty in
  let ml_exp = Ast_helper.Exp.constraint_ ml_exp ml_ty in
  let sy_exp = LiquidFromOCaml.translate_expression env ml_exp in
  let tenv = empty_typecheck_env ~warnings:true contract_sig env in
  let ty_exp = LiquidCheck.typecheck_code tenv ~expected_ty:ty sy_exp in
  let enc_exp = LiquidEncode.encode_code tenv ty_exp in
  let loc = LiquidLoc.loc_in_file env.filename in
  translate_const_exp loc enc_exp

let data_of_liq ~filename ~contract ~parameter ~storage =
  (* first, extract the types *)
  let ocaml_ast = LiquidFromOCaml.structure_of_string
                    ~filename contract in
  let contract, _, env = LiquidFromOCaml.translate ~filename ocaml_ast in
  let _ = LiquidCheck.typecheck_contract
      ~warnings:true env contract in
  let translate filename s ty =
    try
      let c = translate { env with filename } contract.contract_sig s ty in
      let s = LiquidPrinter.Michelson.line_of_const c in
      Ok s
    with LiquidError error ->
      Error error
  in
  (translate "parameter" parameter contract.contract_sig.parameter),
  (translate "storage" storage contract.contract_sig.storage)


let string_of_const ?ty c =
  let e = LiquidToOCaml.convert_const c in
  let e = match ty with
    | None -> e
    | Some ty ->
      Ast_helper.Exp.constraint_ e (LiquidToOCaml.convert_type ty)
  in
  LiquidToOCaml.string_of_expression e
