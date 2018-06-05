(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let rec compute decompile code to_inline =

  let to_inline = ref (if decompile then StringMap.empty else to_inline) in

  (* Do not inline terms larger than this value when decompiling *)
  let inline_treshold_low = 100 in

  let rec size exp =
    match exp.desc with
    | Const _ | Var _ | SetVar _ | Failwith _ -> 1
    | Constructor (_, _, e) -> size e

    | Seq (e1, e2) -> size e1 + size e2

    | Let (_, _, e1, e2) -> size e1

    | Loop (_, _, e1, e2) -> 30 + size e1 + size e2

    | If (e1, e2, e3)
    | MatchNat (e1, _, _, e2, _, e3) -> 40 + size e1 + size e3 + size e3

    | MatchList (e1, _, _, _, e2, e3)
    | MatchOption (e1, _, e2, _, e3) -> 40 + size e1 + size e3 + size e3

    | Fold (_, _, _, e1, e2, e3) -> 30 + size e1 + size e3 + size e3
    | Transfer (_, e1, e2, e3) -> 1 + size e1 + size e3 + size e3

    | Apply (prim, _, l) ->
      List.fold_left (fun acc e -> acc + size e) 1 l

    | Lambda (_, _, _, e, _)
    | Closure (_, _, _, _, e, _) -> 70 + size e

    | Record (_, labels) ->
      List.fold_left (fun acc (_, e) -> acc + size e) 1 labels

    | MatchVariant (e, _, cases) ->
      List.fold_left (fun acc (_, e) -> acc + size e) (size e + 40) cases

    | CreateContract (_, l, _) ->
      List.fold_left (fun acc e -> acc + size e) 1 l
  in

  let rec iter exp =
    match exp.desc with
    | Const _ -> exp
    | Var (name, _loc, []) ->
       begin
         try
           let v = StringMap.find name !to_inline in
           if not decompile then assert (not v.fail);
           iter v
         with Not_found -> exp
       end
    | Var (name, _loc, _::_) -> assert false
    | SetVar (name, _loc, _, _) -> assert false
    | Let (name, loc, v, { desc = Var (vname, _loc, []) })
      when vname = name -> (* special case for let x = e in x *)
      iter v
    | Let (name, loc, v, body) ->
      if decompile && v.name = None && not v.fail && not v.transfer
         && size body <= inline_treshold_low
      then
        to_inline := StringMap.add name v !to_inline;
      let body = iter body in
      if StringMap.mem name !to_inline then
        body
      else
        let v = iter v in
        begin
          try
            if StringSet.mem name (LiquidBoundVariables.bv body) then
              raise Exit;
            if not v.fail then
              body
            else
            if v.ty <> Tunit then raise Exit
            else
              { exp with desc = Seq(v, body); name = None }
          with Exit ->
            { exp with desc = Let(name, loc, v, body) }
        end

    | MatchOption(arg, loc, ifnone, name, ifsome) ->
       let arg = iter arg in
       let ifnone = iter ifnone in
       let ifsome = iter ifsome in
       { exp with desc = MatchOption(arg,loc,ifnone,name,ifsome) }

    | MatchNat(arg, loc, p, ifplus, m, ifminus) ->
       let arg = iter arg in
       let ifplus = iter ifplus in
       let ifminus = iter ifminus in
       { exp with desc = MatchNat(arg, loc, p, ifplus, m, ifminus) }

    | MatchList(arg, loc, head_name, tail_name, ifcons, ifnil) ->
       let arg = iter arg in
       let ifcons = iter ifcons in
       let ifnil = iter ifnil in
       { exp with desc = MatchList(arg,loc,
                                   head_name, tail_name, ifcons,
                                   ifnil) }

    | MatchVariant(arg, loc, cases) ->
       let arg = iter arg in
       let cases = List.map (fun (pat, e) -> pat, iter e) cases in
       { exp with desc = MatchVariant(arg,loc, cases) }

    | Loop(name, loc, body, arg) ->
       let body = iter body in
       let arg = iter arg in
       { exp with desc = Loop(name, loc, body, arg) }

    | Fold(prim, name, loc, body, arg, acc) ->
       let body = iter body in
       let arg = iter arg in
       let acc = iter acc in
       { exp with desc = Fold(prim, name, loc, body, arg, acc) }

    | Seq(e1, e2) ->
       let e1 = iter e1 in
       let e2 = iter e2 in
       if e1.ty = Tfail (* e1 always fails *)
       then e1
       else if not e1.fail && not e1.transfer (* no side-effects *)
       then e2
       else { exp with desc = Seq(e1,e2) }

    | If(cond, ifthen, ifelse) ->
       let cond = iter cond in
       let ifthen = iter ifthen in
       let ifelse = iter ifelse in
       { exp with desc = If(cond, ifthen, ifelse) }

    | Apply(prim, loc, args) ->
       let args = List.map iter args in
       { exp with desc = Apply(prim, loc, args) }

    | Transfer (loc, contract_exp, tez_exp, arg_exp) ->
       let contract_exp = iter contract_exp in
       let tez_exp = iter tez_exp in
       let arg_exp = iter arg_exp in
       { exp with desc = Transfer (loc, contract_exp, tez_exp, arg_exp) }

    | Lambda (arg_name, arg_type, loc, body_exp, res_type) ->
      let body_exp = iter body_exp in
      { exp with
        desc = Lambda (arg_name, arg_type, loc, body_exp, res_type) }

    | Closure (arg_name, arg_type, loc, call_env, body_exp, res_type) ->
      let body_exp = iter body_exp in
      let call_env = List.map (fun (name, t) -> name, iter t) call_env in
      { exp with
        desc = Closure (arg_name, arg_type, loc, call_env, body_exp, res_type) }

    | Record (loc, lab_x_exp_list) ->
       let lab_x_exp_list = List.map (fun (label, exp) -> label, iter exp)
                                     lab_x_exp_list in
       { exp with desc = Record(loc, lab_x_exp_list) }

    | Failwith (_, _) -> exp

    | CreateContract (loc, args, contract) ->
      let args = List.map iter args in
      let contract =
        simplify_contract ~decompile_annoted:decompile contract !to_inline
      in
      { exp with desc = CreateContract(loc, args, contract) }

    | Constructor _ -> assert false (* never found in typed_exp *)
  in

  iter code

and simplify_contract ?(decompile_annoted=false) contract to_inline =
  { contract with code = compute decompile_annoted contract.code to_inline }
