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

let c_empty_op ~loc = mk (Const (loc, Tlist Toperation, CList [])) ()
let mk_nat ~loc i =
  mk (Const (loc, Tnat, CNat (LiquidPrinter.integer_of_int i))) ()

let rec subst_empty_big_map code =
  let empty_big_map loc =
    let storage_var = mk (Var ("storage", loc, [])) () in
    Apply (Prim_tuple_get, loc, [storage_var; mk_nat ~loc 0])
  in
  let desc = code.desc in
  let desc = match desc with
    | Const (loc, ty, CBigMap []) ->
      empty_big_map loc
    | Const (loc, ty, CBigMap _) ->
      LiquidLoc.raise_error ~loc
        "Only use empty big map constants in storage initializer"
    | Const _ -> desc
    | Transfer _ -> assert false
    | Var _
    | Failwith _ -> desc
    | SetVar (s, loc, l, e) ->
      let e' = subst_empty_big_map e in
      if e == e' then desc else SetVar (s, loc, l, e')
    | Constructor (loc, c, e) ->
      let e' = subst_empty_big_map e in
      if e == e' then desc else Constructor (loc, c, e')

    | Lambda (s, t, loc, e, tr) ->
      let e' = subst_empty_big_map e in
      if e == e' then desc else Lambda (s, t, loc, e, tr)

    | Seq (e1, e2) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc else Seq (e1', e2')

    | Let (s, loc, e1, e2) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc else Let (s, loc, e1', e2')

    | Loop (s, loc, e1, e2) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc else Loop (s, loc, e1', e2')

    | If (e1, e2, e3) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else If (e1', e2', e3')

    | MatchOption (e1, loc, e2, s, e3) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchOption (e1', loc, e2', s, e3')

    | MatchNat (e1, loc, s, e2, r, e3) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchNat (e1', loc, s, e2', r, e3')

    | MatchList (e1, loc, s, r, e2, e3) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MatchList (e1', loc, s, r, e2', e3')

    | Fold (c, loc, s, e1, e2, e3) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else Fold (c, loc, s, e1', e2', e3')

    | Map (c, loc, s, e1, e2) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      if e1 == e1' && e2 == e2' then desc
      else Map (c, loc, s, e1', e2')

    | MapFold (c, loc, s, e1, e2, e3) ->
      let e1' = subst_empty_big_map e1 in
      let e2' = subst_empty_big_map e2 in
      let e3' = subst_empty_big_map e3 in
      if e1 == e1' && e2 == e2' && e3 == e3' then desc
      else MapFold (c, loc, s, e1', e2', e3')

    | Apply (p, loc, l) ->
      let l' = List.map subst_empty_big_map l in
      if List.for_all2 (==) l l' then desc
      else Apply (p, loc, l')

    | Closure (s, t, loc, env, e, tr) ->
      let e' = subst_empty_big_map e in
      let env' = List.map (fun (x, e) -> x, subst_empty_big_map e) env in
      if e == e' &&
         List.for_all2 (fun (_, e) (_, e') -> e == e') env env'
      then desc
      else Closure (s, t, loc, env', e', tr)

    | Record (r, l) ->
      let l' = List.map (fun (x, e) -> x, subst_empty_big_map e) l in
      if List.for_all2 (fun (_, e) (_, e') -> e == e') l l'
      then desc
      else Record (r, l')

    | MatchVariant (e, loc, l) ->
      let e' = subst_empty_big_map e in
      let l' = List.map (fun (x, e) -> x, subst_empty_big_map e) l in
      if e == e' && List.for_all2 (fun (_, e) (_, e') -> e == e') l l'
      then desc
      else MatchVariant (e', loc, l')

    | CreateContract (loc, l, contract) ->
      let l' = List.map subst_empty_big_map l in
      if List.for_all2 (==) l l' then desc
      else CreateContract (loc, l', contract)

  in
  if desc == code.desc then
    code
  else
    { code with desc }




let tmp_contract_of_init ~loc (args, code) storage_ty =
  let storage = storage_ty in
  let parameter_var = mk (Var ("parameter", loc, [])) () in
  let parameter, code = match args with
    | [] -> Tunit, code
    | [arg, loc, ty] ->
      let code = mk (Let (arg, loc, parameter_var, code)) () in
      ty, code
    | _ ->
      let parameter = Ttuple (List.map (fun (_,_,ty) -> ty) args) in
      let code, _ = List.fold_right (fun (arg, loc, ty) (code, i) ->
          let i = i - 1 in
          let code = mk (
              Let (arg, loc,
                   mk (Apply
                         (Prim_tuple_get, loc, [parameter_var; mk_nat ~loc i]))
                     (),
                   code)) ()
          in
          (code, i)
        ) args (code, List.length args)
      in
      parameter, code
  in
  (* Empty big map is fetched in given storage which is always empty *)
  let code = subst_empty_big_map code in
  let code =
    mk(Apply (Prim_tuple, loc, [ c_empty_op ~loc; code ])) () in
  let contract_sig = { parameter; storage } in
  { contract_sig; code }

let compile_liquid_init env contract ((args, sy_init) as init) =
  let loc = LiquidCheck.loc_exp sy_init in
  if sy_init.transfer then
    LiquidLoc.raise_error ~loc
      "No transfer allowed in storage initializer";
  try (* Maybe it is constant *)
    let tenv = empty_typecheck_env ~warnings:true contract env in
    let ty_init = LiquidCheck.typecheck_code tenv
        ~expected_ty:contract.storage sy_init in
    let enc_init = LiquidEncode.encode_code tenv ty_init in
    let c_init = LiquidData.translate_const_exp loc enc_init in
    Init_constant c_init
  (* let s = LiquidPrinter.Michelson.line_of_const c_init in
   * let output = env.filename ^ ".init.tz" in
   * FileString.write_file output s;
   * Printf.eprintf "Constant initial storage generated in %S\n%!" output *)
  with LiquidError _ ->
    (* non constant initial value *)
    let init_contract = tmp_contract_of_init ~loc init contract.storage in
    let typed_init = LiquidCheck.typecheck_contract
        ~warnings:true env init_contract in
    let encoded_init, _ = LiquidEncode.encode_contract env typed_init in
    let pre_init = LiquidMichelson.translate encoded_init in
    Init_code (init_contract, pre_init)
(* let mic_init = LiquidToTezos.convert_contract pre_init in
 * let s = LiquidToTezos.line_of_contract mic_init in
 * let output = env.filename ^ ".initializer.tz" in
 * FileString.write_file output s;
 * Printf.eprintf "Storage initializer generated in %S\n%!" output *)
