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

open LiquidTypes

let noloc = LiquidLoc.noloc

let mk ?name ~loc desc = mk ?name ~loc desc ()

let const_name_of_datatype = function
  | Tunit -> "u"
  | Tbool -> "b"
  | Tint -> "i"
  | Tnat -> "n"
  | Ttez -> "amount"
  | Tstring -> "s"
  | Tbytes -> "by"
  | Ttimestamp -> "time"
  | Tkey -> "key"
  | Tkey_hash -> "key_hash"
  | Tsignature -> "sig"
  | Ttuple _ -> "tuple"
  | Toption _ -> "opt"
  | Trecord _ -> "record"
  | Tsum _ -> "sum"
  | Tlist _ -> "l"
  | Tset _ -> "s"
  | Tmap _ -> "map"
  | Tbigmap _ -> "bigmap"
  | Tcontract _ -> "contrat"
  | Tor _ -> "or"
  | Tlambda _ | Tclosure _  -> "fun"
  | Tfail -> "fail"
  | Toperation -> "op"
  | Taddress -> "addr"
  | Tvar _ | Tpartial _ -> assert false


let vars_nums = Hashtbl.create 101
let vars_names = Hashtbl.create 101

let rec var_of node =
  try Hashtbl.find vars_names node.num
  with Not_found ->
  match node.kind with
  (* | N_VAR name -> name *)
  | _ -> match node.node_name with

    | Some name ->
      begin try
          if Hashtbl.find vars_nums name = node.num then name
          else Printf.sprintf "%s%d" name node.num
        with Not_found -> name
      end
    | None -> match node.kind with

      | N_VAR name -> name
      | N_PRIM "CAR" ->
        begin match node.args with
          | [n] -> Printf.sprintf "%s_fst%d" (var_of n) node.num
          | _ -> assert false
        end
      | N_PRIM "CDR" ->
        begin match node.args with
          | [n] -> Printf.sprintf "%s_snd%d" (var_of n) node.num
          | _ -> assert false
        end
      | N_PRIM p -> Printf.sprintf "%s%d" (String.lowercase_ascii p) node.num
      | N_IF _ -> Printf.sprintf "res%d" node.num
      | N_IF_THEN _ -> Printf.sprintf "then%d" node.num
      | N_IF_ELSE _ -> Printf.sprintf "else%d" node.num
      | N_IF_NIL _ -> Printf.sprintf "if_nil%d" node.num
      | N_IF_CONS _ -> Printf.sprintf "if_cons%d" node.num
      | N_IF_LEFT _ -> Printf.sprintf "if_left%d" node.num
      | N_IF_RIGHT _ -> Printf.sprintf "if_right%d" node.num
      | N_IF_PLUS _ -> Printf.sprintf "if_plus%d" node.num
      | N_IF_MINUS _ -> Printf.sprintf "if_minus%d" node.num
      | N_LEFT _ -> Printf.sprintf "left%d" node.num
      | N_CONTRACT _ -> Printf.sprintf "contract%d" node.num
      | N_UNPACK _ -> Printf.sprintf "unpacked%d" node.num
      | N_RIGHT _ -> Printf.sprintf "right%d" node.num
      | N_TRANSFER -> Printf.sprintf "transfer%d" node.num
      | N_IF_RESULT _ | N_IF_END_RESULT _
      | N_LOOP_RESULT _ | N_LOOP_LEFT_END _ | N_LOOP_LEFT_RESULT _ ->
        Printf.sprintf "var%d" node.num
      | N_FAILWITH -> Printf.sprintf "fail%d" node.num
      | N_LOOP _ -> Printf.sprintf "loop%d" node.num
      | N_LOOP_LEFT _ -> Printf.sprintf "loopleft%d" node.num
      | N_LAMBDA _ -> Printf.sprintf "fun%d" node.num
      | N_LAMBDA_BEGIN | N_LOOP_BEGIN _ | N_FOLD_BEGIN _
      | N_LOOP_LEFT_BEGIN _ ->
        Printf.sprintf "arg%d" node.num
      | N_CONST (ty, _) ->
        Printf.sprintf "%s%d" (const_name_of_datatype ty) node.num
      | _ -> Printf.sprintf "exp%d" node.num


let var_of node =
  let name = var_of node in
  Hashtbl.add vars_nums name node.num;
  Hashtbl.add vars_names node.num name;
  name

let lvar_of node =
  let name = var_of node in
  { nname = name; nloc = node.loc }


let nat_n ~loc n =
  mk ~loc (Const { ty = Tnat; const = CNat (LiquidNumber.integer_of_int n) })
let nat_zero = nat_n 0
let nat_one = nat_n 1

let int_n ~loc n =
  mk ~loc (Const { ty = Tint; const = CInt (LiquidNumber.integer_of_int n) })
let int_zero = int_n 0
let int_one = int_n 1

let unit ~loc = mk ~loc (Const { ty =  Tunit; const = CUnit })

let mk_get ~loc ?name arg n =
  mk ?name ~loc
    (Apply { prim = Prim_tuple_get; args = [arg ; nat_n ~loc n ] })



let rec arg_of node =
  let loc = node.loc in
  match node.kind with
  | N_IF_END_RESULT ({ kind = N_IF_END (if_node, _);
                       args }, _end_else_node, pos ) ->
    begin
      match pos, List.length args with
      | 0, 1 -> arg_of if_node
      | _ -> mk_get ~loc (arg_of if_node) pos
    end
  | N_ARG ({ kind = N_LOOP_BEGIN ( _); args } as begin_node, pos ) ->
    begin
      match pos, List.length args with
      | 0, 1 -> arg_of begin_node
      | _ -> mk_get ~loc (arg_of begin_node) pos
    end
  | N_LOOP_RESULT (loop_node, begin_node, pos ) ->
    begin
      match pos, List.length begin_node.args with
      | 0, 1 -> arg_of loop_node
      | _ -> mk_get ~loc (arg_of loop_node) pos
    end

  | N_ARG ({ kind = N_LOOP_LEFT_BEGIN _; args = acc } as begin_node, pos ) ->
    let x_acc = arg_of begin_node in
    begin
      match pos, acc with
      | 0, [] -> (* no accumulator *)
        x_acc
      | 0, _ -> (* arg is left element *)
        mk_get ?name:node.node_name ~loc x_acc 0
      | 1, [_] -> (* arg is accumulator *)
        mk_get ?name:node.node_name ~loc x_acc 1
      | _, _ when pos > 0 -> (* arg in accumulator *)
        let acc_liq = mk_get ~loc x_acc 1 in
        mk_get ?name:node.node_name ~loc acc_liq (pos - 1)
      | _ -> assert false
    end
  | N_LOOP_LEFT_RESULT (loop_node, end_node, pos ) ->
    begin match pos, end_node.args with
      | 0, [] -> arg_of loop_node (* no acc *)
      | _ -> mk_get ~loc (arg_of loop_node) pos
    end

  | N_ARG ({ kind = N_FOLD_BEGIN ( _); args = acc } as begin_node, pos ) ->
    let x_acc = arg_of begin_node in
    begin
      match pos, acc with
      | 0, [] -> x_acc
      | 0, [ { kind = N_CONST (_, CUnit)}] -> (* iter *) x_acc
      | 0, _ -> (* arg is iterated element *)
        mk_get ?name:node.node_name ~loc x_acc 0
      | 1, [_] -> (* arg is accumulator *)
        mk_get ?name:node.node_name ~loc x_acc 1
      | _, _ when pos > 0 -> (* arg in accumulator *)
        let acc_liq = mk_get ~loc x_acc 1 in
        mk_get ?name:node.node_name ~loc acc_liq (pos - 1)
      | _ -> assert false
    end
  | N_FOLD_RESULT (fold_node, end_node, pos ) ->
    begin
      match pos, List.length end_node.args with
      | 0, 1 -> arg_of fold_node
      | _ -> mk_get ~loc (arg_of fold_node) pos
    end

  | N_ARG ({ kind = N_MAP_BEGIN ( _); args = [] } as begin_node, 0 ) ->
    (* .map *)
    arg_of begin_node

  | N_ARG ({ kind = N_MAP_BEGIN ( _); args = acc } as begin_node, pos ) ->
    (* .map_fold *)
    let x_acc = arg_of begin_node in
    begin
      match pos, acc with
      | _, [] -> assert false
      | 0, _ -> (* arg is map element *)
        mk_get ?name:node.node_name ~loc x_acc 0
      | 1, [_] -> (* arg is accumulator *)
        mk_get ?name:node.node_name ~loc x_acc 1
      | _, _ when pos > 0 -> (* arg in accumulator *)
        let acc_liq = mk_get ~loc x_acc 1 in
        mk_get ?name:node.node_name ~loc acc_liq (pos - 1)
      | _ -> assert false
    end

  | N_MAP_RESULT (map_node, end_node, pos ) ->
    begin
      match pos, end_node.args with
      | 0, [] -> arg_of map_node
      | _ -> mk_get ~loc (arg_of map_node) pos
    end

  | N_RESULT (node, pos) -> mk_get ~loc (arg_of node) pos

  | N_CONST (ty, ((
      CUnit | CBool _ | CInt _ | CNat _ | CTez _
    ) as const)) ->
    mk ~loc (Const { ty; const })

  | _ -> mk ~loc (Var (var_of node))

let rec decompile_next (env : env) node =
  let loc = node.loc in
  match node.next with
  | None ->
    Format.eprintf "%s@." (LiquidPrinter.string_of_node node);
    assert false
  | Some node ->
    match node.kind, node.args with
    | N_PRIM "MOD", [arg1; arg2] ->
      mklet env node (MatchOption {
          arg = mk ~loc (Apply { prim = Prim_ediv;
                                 args = [arg_of arg1; arg_of arg2] });
          ifnone = mk ~loc (Failwith (unit ~loc));
          some_name = lvar_of node;
          ifsome = mk_get ~loc (mk ~loc (Var (var_of node))) 1;
        })
    | N_PRIM "DIV", [arg1; arg2] ->
      mklet env node (MatchOption {
          arg = mk ~loc (Apply { prim = Prim_ediv;
                                 args = [arg_of arg1; arg_of arg2] });
          ifnone = mk ~loc (Failwith (unit ~loc));
          some_name = lvar_of node;
          ifsome = mk_get ~loc (mk ~loc (Var (var_of node))) 0;
        })

    (* ABS : int -> int *)
    | N_ABS, [arg] ->
      mklet env node (Apply { prim = Prim_abs; args = [arg_of arg] })

    (* ABS as match%nat *)
    | N_PRIM "ABS", [arg] ->
      let name = var_of arg in
      let v = mk ~loc (Var name) in
      mklet env node ( MatchNat {
          arg = arg_of arg;
          plus_name = { nname = name; nloc = arg.loc };
          ifplus = v;
          minus_name = { nname = name; nloc = arg.loc };
          ifminus = v })

    (* UPDATE true -> Set.add *)
    | N_PRIM "UPDATE", [arg1;
                        { kind = N_CONST (_, CBool true) };
                        arg3] ->
      let arg1, arg3 = arg_of arg1, arg_of arg3 in
      mklet env node (Apply { prim = Prim_set_add; args = [arg1; arg3] })

    (* UPDATE false -> Set.remove *)
    | N_PRIM "UPDATE", [arg1;
                        { kind = N_CONST (_, CBool false) };
                        arg3] ->
      let arg1, arg3 = arg_of arg1, arg_of arg3 in
      mklet env node (Apply { prim = Prim_set_remove; args = [arg1; arg3] })

    (* UPDATE None -> Map.remove *)
    | N_PRIM "UPDATE", [arg1;
                        { kind = N_CONST (_, CNone) };
                        arg3] ->
      let arg1, arg3 = arg_of arg1, arg_of arg3 in
      mklet env node (Apply { prim = Prim_map_remove; args = [arg1; arg3] })

    (* UPDATE Some -> Map.add *)
    | N_PRIM "UPDATE", [arg1;
                        { kind = N_CONST (Toption cty, CSome c) };
                        arg3] ->
      let arg1, arg3 = arg_of arg1, arg_of arg3 in
      let v = mk ~loc (Const { ty = cty; const = decompile_const env c }) in
      mklet env node (Apply { prim = Prim_map_add; args = [arg1; v; arg3] })

    (* UPDATE Some -> Map.add *)
    | N_PRIM "UPDATE", [arg1;
                        { kind = N_PRIM "SOME"; args = [arg2]};
                        arg3] ->
      let arg1, arg2, arg3 = arg_of arg1, arg_of arg2, arg_of arg3 in
      mklet env node (Apply { prim = Prim_map_add;
                          args = [arg1; arg2; arg3] })

    | N_PRIM "PAIR", [{ kind = N_LAMBDA _ } as f; cenv]  ->
      begin match f.node_name with
        | None -> f.node_name <-node.node_name
        | Some _ -> ()
      end;
      let f = arg_of f in
      let cenv = arg_of cenv in
      mklet env node (Apply { prim = Prim_tuple; args =  [f; cenv] })

    | N_PRIM "EXEC", [x; f]  ->
      let x = arg_of x in
      let f = arg_of f in
      mklet env node (Apply { prim = Prim_exec true; args =  [f; x] })

    | N_PRIM prim, _ ->
      let prim, args =
        match prim, node.args with
        | "CDR", [arg] -> Prim_tuple_get, [arg_of arg; nat_one ~loc]
        | "CAR", [arg] -> Prim_tuple_get, [arg_of arg; nat_zero ~loc]
        | "NEQ", [arg] -> Prim_neq, [arg_of arg; int_zero ~loc]
        | "EQ", [arg] -> Prim_eq, [arg_of arg; int_zero ~loc]
        | "GE", [arg] -> Prim_ge, [arg_of arg; int_zero ~loc]
        | "GT", [arg] -> Prim_gt, [arg_of arg; int_zero ~loc]
        | "LE", [arg] -> Prim_le, [arg_of arg; int_zero ~loc]
        | "LT", [arg] -> Prim_lt, [arg_of arg; int_zero ~loc]
        | "NEQ", [arg1;arg2] -> Prim_neq, [arg_of arg1; arg_of arg2]
        | "EQ", [arg1;arg2] -> Prim_eq, [arg_of arg1; arg_of arg2]
        | "GE", [arg1;arg2] -> Prim_ge, [arg_of arg1; arg_of arg2]
        | "GT", [arg1;arg2] -> Prim_gt, [arg_of arg1; arg_of arg2]
        | "LE", [arg1;arg2] -> Prim_le, [arg_of arg1; arg_of arg2]
        | "LT", [arg1;arg2] -> Prim_lt, [arg_of arg1; arg_of arg2]
        | "NOW", [] -> Prim_now, [unit ~loc]
        | "BALANCE", [] -> Prim_balance, [unit ~loc]
        | "AMOUNT",[] -> Prim_amount, [unit ~loc]
        | "STEPS_TO_QUOTA",[] -> Prim_gas, [unit ~loc]
        | "SOURCE",[] -> Prim_source, [unit ~loc]
        | "SENDER",[] -> Prim_sender, [unit ~loc]
        | "SELF",[] -> Prim_self, [unit ~loc]
        | prim, args ->
          let prim =
            match prim with
            | "GET" -> Prim_map_find
            | "PAIR" -> Prim_tuple
            | "COMPARE" -> Prim_compare
            | "CONCAT" -> Prim_concat
            | "SLICE" -> Prim_slice
            | "UPDATE" -> Prim_coll_update
            | "MEM" -> Prim_coll_mem
            | "SIZE" -> Prim_coll_size
            | "CONS" -> Prim_Cons
            | "SUB" -> Prim_sub
            | "ADD" -> Prim_add
            | "MUL" -> Prim_mul
            | "EDIV" -> Prim_ediv
            | "NEG" -> Prim_neg
            | "INT" -> Prim_int
            | "ISNAT" -> Prim_is_nat
            | "PACK" -> Prim_pack
            | "BLAKE2B" -> Prim_blake2b
            | "SHA256" -> Prim_sha256
            | "SHA512" -> Prim_sha512
            | "HASH_KEY" -> Prim_hash_key
            | "CHECK_SIGNATURE" -> Prim_check
            | "CREATE_ACCOUNT" -> Prim_create_account
            | "ADDRESS" -> Prim_address
            | "XOR" -> Prim_xor
            | "NOT" -> Prim_not
            | "OR" -> Prim_or
            | "AND" -> Prim_and
            | "LSR" -> Prim_lsr
            | "LSL" -> Prim_lsl
            | "IMPLICIT_ACCOUNT" -> Prim_default_account
            | "SET_DELEGATE" -> Prim_set_delegate
            | "SOME" -> Prim_Some
            | ins ->
              LiquidLoc.raise_error
                "Error: unknown instruction %S"
                ins
          in
          (prim, List.map arg_of args)
      in
      mklet env node (Apply { prim; args })

    | N_PROJ field, [arg] ->
      mklet env node (Project { field; record = arg_of arg })

    | N_RECORD labels, args ->
      let fields = try
          List.map2 (fun label arg ->
              label, arg_of arg
            ) labels args
        with Invalid_argument _ ->
          LiquidLoc.raise_error
            "Error: Annotatated record construct has %d fields, \
             given %d arguments" (List.length labels) (List.length args)
      in
      mklet env node (Record fields)

    | N_SETFIELD field, [x; y] ->
      let set_val = arg_of x in
      let record = arg_of y in
      mklet env node (SetField { record; field; set_val })

    | N_LEFT right_ty, [arg] ->
      mklet env node
        (Constructor {constr = Left right_ty; arg = arg_of arg })

    | N_RIGHT left_ty, [arg] ->
      mklet env node
        (Constructor {constr = Right left_ty; arg = arg_of arg })

    | N_CONSTR c, [arg] ->
      mklet env node (Constructor {constr = Constr c; arg = arg_of arg })

    | N_CONTRACT ty, [arg] ->
      mklet env node (ContractAt { arg = arg_of arg;
                               c_sig = contract_sig_of_param ty })

    | N_UNPACK ty, [arg] ->
      mklet env node (Unpack { arg = arg_of arg; ty })

    | N_END, [ arg ] -> arg_of arg

    | N_FAILWITH, [ arg ] ->
      mk ~loc (Failwith (arg_of arg))

    | N_CONST (ty, const), [] ->
      let const = decompile_const env const in
      mklet env node (Const { ty; const })

    | N_IF ({ kind = N_IF_END (_, then_node) },
            { kind = N_IF_END (_, else_node) }), [arg] ->
      let desc =
        match then_node.kind, else_node.kind with
        | N_IF_THEN (_), N_IF_ELSE (_) ->
          If { cond = arg_of arg;
               ifthen = decompile_next env then_node;
               ifelse = decompile_next env else_node }
        | N_IF_CONS (_, var0, var1), N_IF_NIL (_) ->
          MatchList { arg = arg_of arg;
                      head_name = lvar_of var0;
                      tail_name = lvar_of var1;
                      ifcons = decompile_next env then_node;
                      ifnil = decompile_next env else_node }
        | N_IF_NONE (_), N_IF_SOME (_,var0) ->
          MatchOption { arg = arg_of arg;
                        ifnone = decompile_next env then_node;
                        some_name = lvar_of var0;
                        ifsome = decompile_next env else_node }
        | N_IF_PLUS (_, var0), N_IF_MINUS (_,var1) ->
          MatchNat { arg = arg_of arg;
                     plus_name = lvar_of var0;
                     ifplus = decompile_next env then_node;
                     minus_name = lvar_of var1;
                     ifminus = decompile_next env else_node }
        | N_IF_LEFT (_, var0), N_IF_RIGHT (_,var1) ->
          MatchVariant { arg = arg_of arg;
                         cases = [
                           PConstr ("Left", [var_of var0]),
                           decompile_next env then_node;
                           PConstr ("Right", [var_of var1]),
                           decompile_next env else_node
                         ] }
        | _ ->
          LiquidLoc.raise_error
            "Error: not implemented at IF node %s%!"
            (LiquidPrinter.string_of_node then_node)
      in
      mklet env node desc
    | N_IF_END _, args -> value_of_args ~loc args

    | N_LOOP (begin_node, end_node), [cond] ->
      let cond_e = arg_of cond in
      let loop_e =
        mk ~loc
          (Loop { arg_name = lvar_of begin_node;
                  body = decompile_next env begin_node;
                  arg = value_of_args ~loc begin_node.args })
      in
      let desc = match cond_e.desc with
        | Const { const = CBool true } ->
          loop_e.desc
        | _ ->
          If { cond = arg_of cond;
               ifthen = loop_e;
               ifelse = value_of_args ~loc begin_node.args }
      in
      mklet env node desc

    | N_LOOP_END (_,_,final_cond), args ->
      mk ~loc (Apply { prim = Prim_tuple;
                       args = [arg_of final_cond;
                               value_of_args ~loc args] })

    | N_LOOP_LEFT (begin_node, end_node), [first] ->
      let arg_name = lvar_of begin_node in
      let acc = match begin_node.args with
        | [] -> None
        | args -> Some (value_of_args ~loc begin_node.args) in
      let desc = match first.kind with
        | N_LEFT _ ->
          LoopLeft { arg_name;
                     body = decompile_next env begin_node;
                     arg = value_of_args ~loc first.args;
                     acc }
        | N_RIGHT _ ->
          (value_of_args ~loc first.args).desc
        | _ ->
          let loop_e =
            mk ~loc
              (LoopLeft { arg_name;
                          body = decompile_next env begin_node;
                          arg = mk ~loc (Var arg_name.nname);
                          acc }) in
          MatchVariant {
            arg = arg_of first;
            cases = [ PConstr ("Left", [arg_name.nname]), loop_e;
                      PConstr ("Right", [arg_name.nname]),
                      value_of_args ~loc first.args ];
          }
      in
      mklet env node desc

    | N_LOOP_LEFT_END (_, _, end_node), args ->
      begin match args with
        | [] -> arg_of end_node
        | _ ->
          mk ~loc (Apply { prim = Prim_tuple;
                           args = [arg_of end_node;
                                   value_of_args ~loc args] })
      end

    | N_FOLD ({args = rargs} as begin_node, end_node), [arg] ->
      let acc = value_of_args ~loc rargs in
      let desc = match acc.desc with
        | Const { const = CUnit } ->
          Fold { prim = Prim_coll_iter;
                 arg_name = lvar_of begin_node;
                 body = decompile_next env begin_node;
                 arg = arg_of arg;
                 acc }
        | _ ->
          Fold { prim = Prim_coll_fold;
                 arg_name = lvar_of begin_node;
                 body = decompile_next env begin_node;
                 arg = arg_of arg;
                 acc }
      in
      mklet env node desc

    | N_FOLD_END (_,_,_), args -> value_of_args ~loc args

    | N_MAP ({args = []} as begin_node, end_node), [arg] ->
      let desc =
        Map { prim = Prim_coll_map;
              arg_name = lvar_of begin_node;
              body = decompile_next env begin_node;
              arg = arg_of arg }
      in
      mklet env node desc

    | N_MAP ({args = rargs} as begin_node, end_node), [arg] ->
      let acc = value_of_args ~loc rargs in
      let desc =
        MapFold { prim = Prim_coll_map_fold;
                  arg_name = lvar_of begin_node;
                  body = decompile_next env begin_node;
                  arg = arg_of arg;
                  acc }
      in
      mklet env node desc

    | N_MAP_END (_,_, res), [] ->
      (* result of .map body *)
      arg_of res

    | N_MAP_END (_,_, res), args ->
      (* result of .map_fold body *)
      mk ~loc (Apply { prim = Prim_tuple;
                       args = [arg_of res; value_of_args ~loc args] })

    | N_LAMBDA (begin_node, end_node, arg_ty, res_ty), [] ->
      let desc = Lambda { arg_name = lvar_of begin_node;
                          arg_ty;
                          body = decompile_next env begin_node;
                          ret_ty = res_ty;
                          recursive = None;
                        }
      in
      mklet env node desc
    | N_LAMBDA_END _, [arg] -> arg_of arg

    | N_TRANSFER, [dest; amount] ->
      mklet env node
        (Transfer { dest = arg_of dest;
                    amount = arg_of amount })

    | N_CALL, [contract; amount; arg] ->
      let entry, arg = match arg.kind, arg.args with
        | N_CONSTR c, [arg] when is_entry_case c ->
          Some (entry_name_of_case c), arg
        | _ -> None, arg in
      mklet env node
        (Call { contract = arg_of contract;
                amount = arg_of amount;
                entry;
                arg = arg_of arg })
    (* TODO *)

    | N_CREATE_CONTRACT contract, args ->
      (* Hack: using annotation to represent contract name *)
      let contract_name =
        match node.node_name with
        | None -> "Contract" ^ string_of_int node.num
        | Some s ->
          try contract_name_of_annot s
          with _ -> "Contract" ^ string_of_int node.num in
      let env = LiquidFromParsetree.mk_inner_env env contract_name in
      let contract = { (decompile env contract) with contract_name } in
      mklet env node
        (CreateContract { args = List.map arg_of args; contract })

    | (
      N_LAMBDA_END _
    | N_LAMBDA _
    | N_TRANSFER
    | N_CALL
    | N_LOOP _
    | N_LOOP_LEFT _
    | N_FOLD _
    | N_MAP _
    | N_IF _
    | N_CONST _
    | N_END
    | N_UNKNOWN _
    | N_LEFT _
    | N_RIGHT _
    | N_CONTRACT _
    | N_UNPACK _
    | N_ABS
    | N_START
    | N_LAMBDA_BEGIN
    | N_VAR _
    | N_FAILWITH
    | N_IF_RESULT (_, _)
    | N_IF_THEN _
    | N_IF_ELSE _
    | N_IF_END_RESULT (_, _, _)
    | N_IF_NONE _
    | N_IF_SOME (_, _)
    | N_IF_NIL _
    | N_IF_CONS (_, _, _)
    | N_IF_LEFT (_, _)
    | N_IF_RIGHT (_, _)
    | N_IF_PLUS (_, _)
    | N_IF_MINUS (_, _)
    | N_LOOP_BEGIN _
    | N_LOOP_LEFT_BEGIN _
    | N_ARG (_, _)
    | N_LOOP_RESULT (_, _, _)
    | N_LOOP_LEFT_RESULT (_, _, _)
    | N_FOLD_BEGIN _
    | N_FOLD_RESULT (_, _, _)
    | N_MAP_BEGIN _
    | N_MAP_RESULT (_, _, _)
    | N_RESULT (_, _)
    | N_PROJ _
    | N_CONSTR _
    | N_SETFIELD _
    ), _->
      LiquidLoc.raise_error
        "not implemented at node %s%!"
        (LiquidPrinter.string_of_node node)

and decompile_const env c = match c with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CContract _ | CSignature _ | CNone  | CKey_hash _
    | CAddress _ ) as c -> c
  | CSome x -> CSome (decompile_const env x)
  | CLeft x -> CLeft (decompile_const env x)
  | CRight x -> CRight (decompile_const env x)
  | CTuple xs -> CTuple (List.map (decompile_const env) xs)
  | CList xs -> CList (List.map (decompile_const env) xs)
  | CSet xs -> CSet (List.map (decompile_const env) xs)
  | CMap l ->
    CMap (List.map (fun (x,y) -> decompile_const env x, decompile_const env y) l)
  | CBigMap l ->
    CBigMap (List.map (fun (x,y) -> decompile_const env x, decompile_const env y) l)
  | CRecord labels ->
    CRecord (List.map (fun (f, x) -> f, decompile_const env x) labels)
  | CConstr (constr, x) ->
    CConstr (constr, decompile_const env x)
  | CLambda { arg_name; arg_ty; body = (begin_node, end_node); ret_ty } ->
    CLambda { arg_name = lvar_of begin_node;
              arg_ty;
              body = decompile_next env begin_node;
              ret_ty;
              recursive = None;
            }

and value_of_args ~loc args =
  match args with
  | [] -> mk ~loc (Const { ty = Tunit; const = CUnit })
  | [arg] -> arg_of arg
  | args ->
    mk ~loc (Apply { prim = Prim_tuple; args = List.map arg_of args })

and mklet env node desc =
  let bnd_val = mk ?name:node.node_name ~loc:node.loc desc in
  let bnd_var = lvar_of node in
  let body = decompile_next env node in
  mk ~loc:node.loc
    (Let { bnd_var; inline = InAuto; bnd_val; body })


and decompile env contract =

  let (begin_node, end_node) = contract.mic_code in

  let parameter_name, storage_name = match begin_node.next with
    | Some { kind = N_PRIM "PAIR"; args = [p; s] } ->
      var_of p, var_of s
    | _ -> "parameter", "storage" in

  let code = decompile_next env begin_node in

  { contract_name = "_dummy_";
    storage = contract.mic_storage;
    values = [];
    entries = [{ entry_sig = { entry_name = "main";
                               parameter = contract.mic_parameter;
                               parameter_name ;
                               storage_name  };
                 code }];
    c_init = None;
    subs = [];
    ty_env = env ;
  }


let decompile env contract =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Decompile Michelson contract@.";
  Hashtbl.reset vars_nums;
  Hashtbl.reset vars_names;
  let ty_env = LiquidFromTezos.convert_env env in
  let contract = decompile ty_env contract in
  { contract with
    ty_env;
    contract_name = ty_env.contractname;
  }

let decompile_const c =
  decompile_const (LiquidFromParsetree.initial_env "_dummy_env_") c
