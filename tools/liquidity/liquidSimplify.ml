(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

(* Simplify a Liquidity contract, mostly inlining and some simple
   simplifications (TODO: add mode simplifications) *)

open LiquidTypes

let rec compute decompile code to_inline =

  let old_to_inline = to_inline in
  let to_inline = ref (if decompile then StringMap.empty else to_inline) in

  (* Do not inline terms larger than this value when decompiling *)
  let inline_treshold_low = 100 in

  let rec size exp =
    match exp.desc with
    | Const _ | Var _ | SetField _ | Project _ -> 1

    | Failwith arg
    | ContractAt { arg }
    | Unpack { arg }
    | Constructor { arg } -> size arg

    | Seq (e1, e2) -> size e1 + size e2

    | Let { bnd_val } -> size bnd_val

    | Loop { arg; body }
    | LoopLeft { arg; body } -> 30 + size arg + size body

    | If { cond = e1 ; ifthen = e2 ; ifelse = e3 }
    | MatchNat { arg = e1; ifplus = e2; ifminus = e3 }
    | MatchList { arg = e1; ifnil = e2; ifcons = e3 }
    | MatchOption { arg = e1; ifnone = e2; ifsome = e3 } ->
      40 + size e1 + size e2 + size e3

    | Map { arg; body } -> 30 + size arg + size body
    | Fold { body; arg; acc }
    | MapFold { body; arg; acc } -> 30 + size body + size arg + size acc
    | Call { contract; amount; arg } ->
      1 + size contract + size amount + size arg
    | Transfer { dest; amount } ->
      1 + size dest + size amount

    | Apply { args } ->
      List.fold_left (fun acc e -> acc + size e) 1 args

    | Lambda { body }
    | Closure { body } -> 70 + size body

    | Record fields ->
      List.fold_left (fun acc (_, e) -> acc + size e) 1 fields

    | MatchVariant { arg; cases } ->
      List.fold_left (fun acc (_, e) -> acc + size e) (size arg + 40) cases

    | CreateContract { args }  ->
      List.fold_left (fun acc e -> acc + size e) 1 args

    | TypeAnnot _ -> 0
    | Type _ -> 0
  in

  let is_const e = match e.desc with Const _ -> true | _ -> false in

  let rec iter exp =
    match exp.desc with
    | Const { ty; const } ->
      { exp with desc = Const { ty; const = iter_const const } }
    | Var name ->
      begin
        try
          let v = StringMap.find name !to_inline in
          iter v
        with Not_found -> exp
      end
    | SetField { record; field; set_val } ->
      let record = iter record in
      let set_val = iter set_val in
      begin match record.desc, set_val.desc with
        | SetField s, Project p
          when s.field <> field && p.field = field &&
               eq_syntax_exp s.record p.record ->
          (* (s.f1 <- v1).f2 <- s.f2  ==>  s.f1 <- v1 *)
          record
        | _, Project p
          when p.field = field && eq_syntax_exp p.record record ->
          (* s.f <- s.f  ==>  s *)
          record
        | _, _ -> { exp with desc = SetField { record; field; set_val } }
      end
    | Project { field; record } ->
      let record = iter record in
      { exp with desc = Project { field; record } }

    | Apply { prim = Prim_tuple_get;
              args = [
                { desc = Apply { prim = Prim_tuple;
                                 args = tuple
                               }
                };
                { desc = Const { const = CInt n | CNat n } }] } ->
      iter (List.nth tuple (LiquidNumber.int_of_integer n))

    | Apply { prim = Prim_tuple_get;
              args = [
                { desc = Const { const = CTuple tuple ; ty = Ttuple tt } };
                { desc = Const { const = CInt n | CNat n } }] } ->
      let n = LiquidNumber.int_of_integer n in
      let const = iter_const (List.nth tuple n) in
      let ty = List.nth tt n in
      { exp with desc = Const { const; ty } }

    | Let { bnd_val = ({ ty = Tfail } as bnd_val) } ->
      iter bnd_val
    | Let { bnd_var; bnd_val; body = { desc = Var name }}
      when name = bnd_var.nname -> (* special case for let x = e in x *)
      iter bnd_val
    | Let { bnd_var; bnd_val = ({ ty = Ttuple tys} as v);
            body = { desc = Apply { prim = Prim_tuple; args = tuple } }}
      when
        let len, ok =
          List.fold_left (fun (i, ok) t -> match t.desc with
              | Apply { prim = Prim_tuple_get;
                        args = [
                          { desc = Var name };
                          { desc = Const { const = CInt n | CNat n } }] } ->
                let ok = ok && name = bnd_var.nname &&
                         LiquidNumber.int_of_integer n = i in
                (i + 1, ok)
              | _ -> (i + 1, false)
            ) (0, true) tuple in
        ok && List.length tys = len
      ->
      (* special case for let x = v in (x.(0), x.(1)) *)
      iter v
    | Let { bnd_var; inline; bnd_val; body } ->
      let restore_to_inline =
        let out_to_inline = !to_inline in
        fun () -> to_inline := out_to_inline
      in
      if decompile && bnd_val.name = None &&
         size bnd_val <= inline_treshold_low &&
         (StringMap.mem bnd_var.nname old_to_inline ||
          match bnd_val.desc with
          | Var _ | Apply { prim = Prim_tuple_get | Prim_tuple } -> true
          | _ -> false)
      then
        to_inline := StringMap.add bnd_var.nname bnd_val !to_inline;
      if is_const bnd_val then
        to_inline := StringMap.add bnd_var.nname bnd_val !to_inline;
      (* let obody = body in *)
      let body = iter body in
      (* if body <> obody then iter { exp with desc = Let (name, loc, v, body) } else *)
      if StringMap.mem bnd_var.nname !to_inline then
        (restore_to_inline ();
         body)
      else
        let () = restore_to_inline () in
        let bnd_val = iter bnd_val in
        let effect = match bnd_val.desc with
          | Lambda _ -> false
          | Closure _ -> false
          | _ -> bnd_val.effect in
        begin
          try
            if StringSet.mem bnd_var.nname (LiquidBoundVariables.bv body) then
              raise Exit;
            if not effect then
              body
            else
            if bnd_val.ty <> Tunit then raise Exit
            else
              { exp with desc = Seq (bnd_val, body); name = None }
          with Exit ->
            { exp with desc = Let { bnd_var; inline; bnd_val; body } }
        end

    | MatchOption { arg; ifnone; some_name; ifsome } ->
      let arg = iter arg in
      let ifnone = iter ifnone in
      let ifsome = iter ifsome in
      { exp with desc = MatchOption { arg; ifnone; some_name; ifsome } }

    | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
      let arg = iter arg in
      let ifplus = iter ifplus in
      let ifminus = iter ifminus in
      { exp with
        desc = MatchNat { arg; plus_name; ifplus; minus_name; ifminus } }

    | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
      let arg = iter arg in
      let ifcons = iter ifcons in
      let ifnil = iter ifnil in
      { exp with
        desc = MatchList { arg; head_name; tail_name; ifcons; ifnil } }

    | MatchVariant { arg; cases } ->
      let arg = iter arg in
      let cases = List.map (fun (pat, e) -> pat, iter e) cases in
      { exp with desc = MatchVariant { arg; cases } }

    | Loop { arg_name; body; arg } ->
      let body = iter body in
      let arg = iter arg in
      { exp with desc = Loop { arg_name; body; arg } }

    | LoopLeft { arg_name; body; arg; acc } ->
      let body = iter body in
      let arg = iter arg in
      let acc = match acc with None -> None | Some acc -> Some (iter acc) in
      { exp with desc = LoopLeft { arg_name; body; arg; acc } }

    | Fold { prim; arg_name; body; arg; acc } ->
      let body = iter body in
      let arg = iter arg in
      let acc = iter acc in
      { exp with desc = Fold { prim; arg_name; body; arg; acc } }

    | Map { prim; arg_name; body; arg } ->
      let body = iter body in
      let arg = iter arg in
      { exp with desc = Map { prim; arg_name; body; arg } }

    | MapFold { prim; arg_name; body; arg; acc } ->
      let body = iter body in
      let arg = iter arg in
      let acc = iter acc in
      { exp with desc = MapFold { prim; arg_name; body; arg; acc } }

    | Seq(e1, e2) ->
      let e1 = iter e1 in
      let e2 = iter e2 in
      if e1.ty = Tfail (* e1 always fails *)
      then e1
      else if not e1.effect && not e1.transfer (* no side-effects *)
      then e2
      else { exp with desc = Seq(e1,e2) }

    | If { cond; ifthen; ifelse } ->
      let cond = iter cond in
      let ifthen = iter ifthen in
      let ifelse = iter ifelse in
      { exp with desc = If { cond; ifthen; ifelse } }

    | Apply { prim = Prim_exec top; args =  [f; x] } ->
      (* inline body of lambda or closures *)
      let f = iter f in
      let x = iter x in
      begin match f.desc with
        | Lambda { arg_name; body } ->
          iter { exp with
                 desc = Let { bnd_var = arg_name;
                              inline = InAuto;
                              bnd_val = x;  body }
               }
        | Closure { arg_name; body; call_env } ->
          let bnd_val =
            mk ~loc:exp.loc
              (Apply { prim = Prim_tuple; args = x :: (List.map snd call_env) })
              (Ttuple (x.ty :: (List.map (fun (_, x) -> x.ty) call_env))) in
          iter { exp with
                 desc = Let { bnd_var = arg_name;
                              inline = InAuto;
                              bnd_val;  body }
               }
        | _ ->
          { exp with desc = Apply { prim = Prim_exec top; args = [f; x] } }
      end

    | Apply { prim; args } ->
      let args = List.map iter args in
      { exp with desc = Apply { prim; args } }

    | Transfer { dest; amount } ->
      let dest = iter dest in
      let amount = iter amount in
      { exp with desc = Transfer { dest; amount } }

    | Call { contract; amount; entry; arg } ->
      let contract = iter contract in
      let amount = iter amount in
      let arg = iter arg in
      { exp with desc = Call { contract; amount; entry; arg } }

    | Lambda { arg_name; arg_ty; body; ret_ty; recursive } ->
      let body = iter body in
      { exp with
        desc = Lambda { arg_name; arg_ty; body; ret_ty; recursive } }

    | Closure { arg_name; arg_ty; call_env; body; ret_ty } ->
      let call_env = List.map (fun (name, t) -> name, iter t) call_env in
      let body = iter body in
      { exp with
        desc = Closure { arg_name; arg_ty; call_env; body; ret_ty } }

    | Record fields ->
      let fields = List.map (fun (label, exp) -> label, iter exp) fields in
      { exp with desc = Record fields }

    | Failwith arg ->
      { exp with desc = Failwith (iter arg) }

    | CreateContract { args; contract } ->
      let args = List.map iter args in
      (* contract is already simplified *)
      { exp with desc = CreateContract { args; contract } }

    | ContractAt { arg; c_sig } ->
      let arg = iter arg in
      { exp with desc = ContractAt { arg; c_sig } }

    | Unpack { arg; ty } ->
      let arg = iter arg in
      { exp with desc = Unpack { arg; ty } }

    | Constructor { constr; arg } ->
      let arg = iter arg in
      { exp with desc = Constructor { constr; arg } }

    | TypeAnnot { e; ty } ->
      let e = iter e in
      { exp with desc = TypeAnnot { e; ty } }

    | Type _ -> exp

  and iter_const c = match c with
    | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
      | CBytes _ | CKey _ | CContract _ | CSignature _ | CNone  | CKey_hash _
      | CAddress _ ) as c -> c
    | CSome x -> CSome (iter_const x)
    | CLeft x -> CLeft (iter_const x)
    | CRight x -> CRight (iter_const x)
    | CTuple xs -> CTuple (List.map (iter_const) xs)
    | CList xs -> CList (List.map (iter_const) xs)
    | CSet xs -> CSet (List.map (iter_const) xs)
    | CMap l ->
      CMap (List.map (fun (x,y) -> iter_const x, iter_const y) l)
    | CBigMap l ->
      CBigMap (List.map (fun (x,y) -> iter_const x, iter_const y) l)
    | CRecord labels ->
      CRecord (List.map (fun (f, x) -> f, iter_const x) labels)
    | CConstr (constr, x) ->
      CConstr (constr, iter_const x)
    | CLambda { arg_name; arg_ty; body; ret_ty; recursive } ->
      let body = iter body in
      CLambda { arg_name; arg_ty; body; ret_ty; recursive }
  in


  let rec fixpoint code =
    let c = iter code in
    if eq_syntax_exp c code then c else fixpoint c
  in

  fixpoint code

(* iter code *)

and simplify_contract ?(decompile_annoted=false) contract (to_inline, to_inline_fee) =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Simplify (inlining) contract %s@."
      (LiquidNamespace.qual_contract_name contract);

  match contract.entries with
  | [{ entry_sig = { entry_name = "main" };
       code; fee_code } as entry ] ->
    { contract with
      entries = [{ entry with
                   code = compute decompile_annoted code to_inline ;
                   fee_code = match fee_code with
                     | None -> None
                     | Some fee_code -> Some (compute decompile_annoted fee_code to_inline)
                 }]
    }
  | _ -> assert false

let simplify_const const =
  let c_exp = mk ~loc:noloc (Const { ty = Tunit ; const }) Tunit in (* dummy *)
  match compute true c_exp StringMap.empty with
  | { desc = Const { const } } -> const
  | _ -> assert false
