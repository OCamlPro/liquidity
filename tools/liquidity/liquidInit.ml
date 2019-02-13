(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

type init =
  | Init_constant of LiquidTypes.const
  | Init_code of (LiquidTypes.encoded_contract *
                  LiquidTypes.loc_michelson_contract)

let c_empty_op ~loc =
  mk ~loc (Const { ty = Tlist Toperation; const = CList []}) (Tlist Toperation)
let mk_nat ~loc i =
  mk ~loc
    (Const { ty = Tnat; const = CNat (LiquidNumber.integer_of_int i) })
    Tnat

let rec subst_empty_big_map storage_ty code =
  let empty_big_map loc =
    let storage_var = mk ~loc (Var "_storage") storage_ty (* dummy *) in
    Apply { prim = Prim_tuple_get; args = [storage_var; mk_nat ~loc 0] }
  in
  let desc = code.desc in
  let loc = code.loc in
  let desc = match desc with
    | Const { const = CBigMap [] } ->
      empty_big_map loc
    | Const { const = CBigMap _ } ->
      LiquidLoc.raise_error ~loc
        "Only use empty big map constants in storage initializer"
    | Const _ -> desc
    | Var _ -> desc

    | Transfer _ | Call _ -> assert false

    | Failwith arg ->
      let arg' = subst_empty_big_map storage_ty arg in
      if arg == arg' then desc else Failwith arg'


    | Project { field; record } ->
      let record' = subst_empty_big_map storage_ty record in
      if record == record' then desc
      else Project { field; record = record' }

    | SetField { record = e1; field; set_val = e2 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      if e1 == e1' && e2 == e2' then desc
      else SetField { record = e1'; field; set_val = e2' }

    | Constructor { constr; arg } ->
      let arg' = subst_empty_big_map storage_ty arg in
      if arg == arg' then desc else Constructor { constr; arg = arg' }

    | Lambda l ->
      let e' = subst_empty_big_map storage_ty l.body in
      if l.body == e' then desc else Lambda { l with body = e' }

    | Seq (e1, e2) ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      if e1 == e1' && e2 == e2' then desc else Seq (e1', e2')

    | Let { bnd_var; inline; bnd_val = e1; body = e2 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      if e1 == e1' && e2 == e2' then desc
      else Let { bnd_var; inline; bnd_val = e1'; body = e2' }

    | Loop { arg_name; body = e1; arg = e2 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      if e1 == e1' && e2 == e2' then desc
      else Loop { arg_name; body = e1'; arg = e2' }

    | LoopLeft { arg_name; body = e1; arg = e2; acc= e3 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      let e3' = match e3 with
        | None -> e3
        | Some e3 -> Some (subst_empty_big_map storage_ty e3) in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else LoopLeft { arg_name; body = e1'; arg = e2'; acc = e3' }

    | If { cond = e1; ifthen = e2; ifelse = e3 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      let e3' = subst_empty_big_map storage_ty e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else If { cond = e1'; ifthen = e2'; ifelse = e3' }

    | MatchOption { arg = e1; ifnone = e2; some_name; ifsome = e3 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      let e3' = subst_empty_big_map storage_ty e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchOption { arg = e1'; ifnone = e2'; some_name; ifsome = e3' }

    | MatchNat { arg = e1;
                 plus_name; ifplus = e2;
                 minus_name; ifminus = e3 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      let e3' = subst_empty_big_map storage_ty e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchNat { arg = e1';
                      plus_name; ifplus = e2';
                      minus_name; ifminus = e3' }

    | MatchList { arg = e1;
                  head_name; tail_name; ifcons = e2; ifnil = e3 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      let e3' = subst_empty_big_map storage_ty e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchList { arg = e1;
                       head_name; tail_name; ifcons = e2'; ifnil = e3' }

    | Fold { prim; arg_name; body = e1; arg = e2; acc = e3 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      let e3' = subst_empty_big_map storage_ty e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else Fold { prim; arg_name; body = e1'; arg = e2'; acc = e3' }

    | Map { prim; arg_name; body = e1; arg = e2 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      if e1 == e1' && e2 == e2' then desc
      else Map { prim; arg_name; body = e1'; arg = e2' }

    | MapFold { prim; arg_name; body = e1; arg = e2; acc = e3 } ->
      let e1' = subst_empty_big_map storage_ty e1 in
      let e2' = subst_empty_big_map storage_ty e2 in
      let e3' = subst_empty_big_map storage_ty e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MapFold { prim; arg_name; body = e1'; arg = e2'; acc = e3' }

    | Apply { prim = ( Prim_sender
                     | Prim_source
                     | Prim_self
                     | Prim_balance
                     | Prim_gas
                     | Prim_create_account ) as p } ->
      LiquidLoc.raise_error ~loc
        "%s forbidden in initializer (for this version of Tezos)"
        (string_of_primitive p)

    | Apply { prim = Prim_unknown;
              args = { desc = Var (("Current.source" | "Curent.sender") as p);
                       loc } :: _ } ->
      LiquidLoc.raise_error ~loc
        "%s forbidden in initializer (for this version of Tezos)" p

    | Apply { prim; args } ->
      let args' = List.map (subst_empty_big_map storage_ty) args in
      if List.for_all2 (==) args args' then desc
      else Apply { prim; args = args' }

    | Closure { arg_name; arg_ty; call_env; body; ret_ty } ->
      let body' = subst_empty_big_map storage_ty body in
      let call_env' =
        List.map (fun (x, e) -> x, subst_empty_big_map storage_ty e) call_env in
      if body == body' &&
         List.for_all2 (fun (_, e) (_, e') -> e == e') call_env call_env'
      then desc
      else Closure { arg_name; arg_ty;
                     call_env = call_env'; body = body'; ret_ty }

    | Record fields ->
      let fields' = List.map (fun (x, e) -> x, subst_empty_big_map storage_ty e) fields in
      if List.for_all2 (fun (_, e) (_, e') -> e == e') fields fields'
      then desc
      else Record fields'

    | MatchVariant { arg; cases } ->
      let arg' = subst_empty_big_map storage_ty arg in
      let cases' = List.map (fun (x, e) -> x, subst_empty_big_map storage_ty e) cases in
      if arg == arg' &&
         List.for_all2 (fun (_, e) (_, e') -> e == e') cases cases'
      then desc
      else MatchVariant { arg = arg'; cases = cases' }

    | CreateContract { args; contract } ->
      let args' = List.map (subst_empty_big_map storage_ty) args in
      if List.for_all2 (==) args args' then desc
      else CreateContract { args = args'; contract }

    | ContractAt { arg; c_sig } ->
      let arg' = subst_empty_big_map storage_ty arg in
      if arg == arg' then desc
      else ContractAt { arg = arg'; c_sig }

    | Unpack { arg; ty } ->
      let arg' = subst_empty_big_map storage_ty arg in
      if arg == arg' then desc
      else Unpack { arg = arg'; ty }

    | TypeAnnot { e; ty } ->
      let e' = subst_empty_big_map storage_ty e in
      if e == e' then desc
      else TypeAnnot { e = e'; ty }

    | Type _ -> desc

  in
  if desc == code.desc then
    code
  else
    { code with desc }




let tmp_contract_of_init ~loc env (init : encoded_exp LiquidTypes.init) storage_ty =
  (* let init =
   *   { init with init_body = (LiquidUntype.untype_code init.init_body : syntax_exp) } in *)
  let storage = storage_ty in
  let parameter, code = match init.init_args with
    | [] -> Tunit, init.init_body
    | [arg, loc, ty] ->
      let parameter_var = mk ~loc (Var "_parameter") ty in
      let code = mk ~loc
          (Let { bnd_var = { nname = arg; nloc = loc };
                 inline = false;
                 bnd_val = parameter_var;
                 body = init.init_body }) init.init_body.ty in
      ty, code
    | args ->
      let parameter = Ttuple (List.map (fun (_,_,ty) -> ty) args) in
      let parameter_var = mk ~loc (Var "_parameter") parameter in
      let code, _ = List.fold_right (fun (arg, loc, ty) (code, i) ->
          let i = i - 1 in
          let code = mk ~loc (
              Let { bnd_var = { nname = arg; nloc = loc };
                    inline = false;
                    bnd_val = mk ~loc (Apply {
                        prim = Prim_tuple_get;
                        args = [parameter_var; mk_nat ~loc i] })
                        ty;
                    body = code }) code.ty
          in
          (code, i)
        ) args (init.init_body, List.length args)
      in
      parameter, code
  in
  (* Empty big map is fetched in given storage which is always empty *)
  let code = subst_empty_big_map storage_ty code in
  let code =
    mk ~loc (Apply { prim = Prim_tuple;
                     args = [ c_empty_op ~loc; code ] })
      (Ttuple [(Tlist Toperation); code.ty]) in
  { contract_name = "_dummy_init";
    storage;
    values = [];
    entries = [{ entry_sig = { entry_name = "main";
                               parameter;
                               parameter_name = "_parameter";
                               storage_name = "_storage" };
                 code }];
    ty_env = env;
    c_init = None;
  }

let compile_liquid_init env contract_sig (init : encoded_exp LiquidTypes.init) (* ((args, sy_init) as init) *) =
  let loc = init.init_body.loc in
  if init.init_body.transfer then
    LiquidLoc.raise_error ~loc
      "No transfer allowed in storage initializer";
  try (* Maybe it is constant *)
    let c_init = LiquidData.translate_const_exp init.init_body in
    Init_constant c_init
  (* let s = LiquidPrinter.Michelson.line_of_const c_init in
   * let output = env.filename ^ ".init.tz" in
   * FileString.write_file output s;
   * Printf.eprintf "Constant initial storage generated in %S\n%!" output *)
  with LiquidError _ ->
    (* non constant initial value *)
    let init_contract = tmp_contract_of_init ~loc env init contract_sig.f_storage in
    let pre_init = LiquidMichelson.translate init_contract in
    Init_code (init_contract, pre_init)
(* let mic_init = LiquidToTezos.convert_contract pre_init in
 * let s = LiquidToTezos.line_of_contract mic_init in
 * let output = env.filename ^ ".initializer.tz" in
 * FileString.write_file output s;
 * Printf.eprintf "Storage initializer generated in %S\n%!" output *)
