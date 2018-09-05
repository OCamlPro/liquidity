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
  | Init_code of (LiquidTypes.syntax_contract *
                  LiquidTypes.loc_michelson_contract)

let c_empty_op ~loc =
  mk (Const { loc; ty = Tlist Toperation; const = CList []})  ()
let mk_nat ~loc i =
  mk
    (Const { loc; ty = Tnat; const = CNat (LiquidPrinter.integer_of_int i) })
    ()

let rec subst_empty_big_map code =
  let empty_big_map loc =
    let storage_var = mk (Var { name = "_storage"; loc }) () in
    Apply { prim = Prim_tuple_get; loc; args = [storage_var; mk_nat ~loc 0] }
  in
  let desc = code.desc in
  let desc = match desc with
    | Const { loc; const = CBigMap [] } ->
      empty_big_map loc
    | Const { loc; const = CBigMap _ } ->
      LiquidLoc.raise_error ~loc
        "Only use empty big map constants in storage initializer"
    | Const _ -> desc
    | Transfer _ -> assert false
    | Var _ -> desc

    | Failwith { arg; loc } ->
      let arg' = subst_empty_big_map arg in
      if arg == arg' then desc else Failwith { arg = arg'; loc }

    | Project { loc; field; record } ->
      let record' = subst_empty_big_map record in
      if record == record' then desc
      else Project { loc; field; record = record' }

    | SetField { record = e1; loc; field; set_val = e2 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc
      else SetField { record = e1'; loc; field; set_val = e2' }

    | Constructor { loc; constr; arg } ->
      let arg' = subst_empty_big_map arg in
      if arg == arg' then desc else Constructor { loc; constr; arg = arg' }

    | Lambda l ->
      let e' = subst_empty_big_map l.body in
      if l.body == e' then desc else Lambda { l with body = e' }

    | Seq (e1, e2) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc else Seq (e1', e2')

    | Let { bnd_var; inline; loc; bnd_val = e1; body = e2 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc
      else Let { bnd_var; inline; loc; bnd_val = e1'; body = e2' }

    | Loop { arg_name; loc; body = e1; arg = e2 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc
      else Loop { arg_name; loc; body = e1'; arg = e2' }

    | If { cond = e1; ifthen = e2; ifelse = e3 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else If { cond = e1'; ifthen = e2'; ifelse = e3' }

    | MatchOption { arg = e1; loc; ifnone = e2; some_name; ifsome = e3 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchOption { arg = e1'; loc; ifnone = e2'; some_name; ifsome = e3' }

    | MatchNat { arg = e1; loc;
                 plus_name; ifplus = e2;
                 minus_name; ifminus = e3 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchNat { arg = e1'; loc;
                      plus_name; ifplus = e2';
                      minus_name; ifminus = e3' }

    | MatchList { arg = e1; loc;
                  head_name; tail_name; ifcons = e2; ifnil = e3 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchList { arg = e1; loc;
                       head_name; tail_name; ifcons = e2'; ifnil = e3' }

    | Fold { prim; arg_name; loc; body = e1; arg = e2; acc = e3 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else Fold { prim; arg_name; loc; body = e1'; arg = e2'; acc = e3' }

    | Map { prim; arg_name; loc; body = e1; arg = e2 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc
      else Map { prim; arg_name; loc; body = e1'; arg = e2' }

    | MapFold { prim; arg_name; loc; body = e1; arg = e2; acc = e3 } ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MapFold { prim; arg_name; loc; body = e1'; arg = e2'; acc = e3' }

    | Apply { prim; loc; args } ->
      let args' = List.map subst_empty_big_map args in
      if List.for_all2 (==) args args' then desc
      else Apply { prim; loc; args = args' }

    | Closure { arg_name; arg_ty; loc; call_env; body; ret_ty } ->
      let body' = subst_empty_big_map body in
      let call_env' =
        List.map (fun (x, e) -> x, subst_empty_big_map e) call_env in
      if body == body' &&
         List.for_all2 (fun (_, e) (_, e') -> e == e') call_env call_env'
      then desc
      else Closure { arg_name; arg_ty; loc;
                     call_env = call_env'; body = body'; ret_ty }

    | Record { loc; fields } ->
      let fields' = List.map (fun (x, e) -> x, subst_empty_big_map e) fields in
      if List.for_all2 (fun (_, e) (_, e') -> e == e') fields fields'
      then desc
      else Record { loc; fields = fields' }

    | MatchVariant { arg; loc; cases } ->
      let arg' = subst_empty_big_map arg in
      let cases' = List.map (fun (x, e) -> x, subst_empty_big_map e) cases in
      if arg == arg' &&
         List.for_all2 (fun (_, e) (_, e') -> e == e') cases cases'
      then desc
      else MatchVariant { arg = arg'; loc; cases = cases' }

    | CreateContract { loc; args; contract } ->
      let args' = List.map subst_empty_big_map args in
      if List.for_all2 (==) args args' then desc
      else CreateContract { loc; args = args'; contract }

    | ContractAt { loc; arg; c_sig } ->
      let arg' = subst_empty_big_map arg in
      if arg == arg' then desc
      else ContractAt { loc; arg = arg'; c_sig }

    | Unpack { loc; arg; ty } ->
      let arg' = subst_empty_big_map arg in
      if arg == arg' then desc
      else Unpack { loc; arg = arg'; ty }

  in
  if desc == code.desc then
    code
  else
    { code with desc }




let tmp_contract_of_init ~loc init storage_ty =
  let storage = storage_ty in
  let parameter_var = mk (Var { name = "_parameter"; loc }) () in
  let parameter, code = match init.init_args with
    | [] -> Tunit, init.init_body
    | [arg, loc, ty] ->
      let code = mk (Let { bnd_var = arg; inline = false; loc;
                           bnd_val = parameter_var;
                           body = init.init_body }) () in
      ty, code
    | args ->
      let parameter = Ttuple (List.map (fun (_,_,ty) -> ty) args) in
      let code, _ = List.fold_right (fun (arg, loc, ty) (code, i) ->
          let i = i - 1 in
          let code = mk (
              Let { bnd_var = arg; inline = false; loc;
                    bnd_val = mk (Apply {
                        prim = Prim_tuple_get; loc;
                        args = [parameter_var; mk_nat ~loc i] })
                        ();
                    body = code }) ()
          in
          (code, i)
        ) args (init.init_body, List.length args)
      in
      parameter, code
  in
  (* Empty big map is fetched in given storage which is always empty *)
  let code = subst_empty_big_map code in
  let code =
    mk (Apply { prim = Prim_tuple; loc;
                args = [ c_empty_op ~loc; code ] }) () in
  { contract_name = "_dummy_init";
    storage;
    values = [];
    entries = [{ entry_sig = { entry_name = "main";
                               parameter;
                               parameter_name = "_parameter";
                               storage_name = "_storage" };
                 code }]
  }

let compile_liquid_init env contract storage_ty init (* ((args, sy_init) as init) *) =
  let loc = LiquidCheck.loc_exp init.init_body in
  if init.init_body.transfer then
    LiquidLoc.raise_error ~loc
      "No transfer allowed in storage initializer";
  try (* Maybe it is constant *)
    let tenv = empty_typecheck_env ~warnings:true contract storage_ty env in
    let ty_init = LiquidCheck.typecheck_code tenv
        ~expected_ty:storage_ty init.init_body in
    let enc_init = LiquidEncode.encode_code tenv ty_init in
    let c_init = LiquidData.translate_const_exp loc enc_init in
    Init_constant c_init
  (* let s = LiquidPrinter.Michelson.line_of_const c_init in
   * let output = env.filename ^ ".init.tz" in
   * FileString.write_file output s;
   * Printf.eprintf "Constant initial storage generated in %S\n%!" output *)
  with LiquidError _ ->
    (* non constant initial value *)
    let init_contract = tmp_contract_of_init ~loc init storage_ty in
    let typed_init = LiquidCheck.typecheck_contract
        ~warnings:true ~decompiling:false env init_contract in
    let encoded_init, _ =
      LiquidEncode.encode_contract env typed_init in
    let pre_init = LiquidMichelson.translate encoded_init in
    Init_code (init_contract, pre_init)
(* let mic_init = LiquidToTezos.convert_contract pre_init in
 * let s = LiquidToTezos.line_of_contract mic_init in
 * let output = env.filename ^ ".initializer.tz" in
 * FileString.write_file output s;
 * Printf.eprintf "Storage initializer generated in %S\n%!" output *)
