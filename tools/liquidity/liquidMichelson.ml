(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes




(*  Translation to Michelson *)



let seq exprs = SEQ exprs

let dup n = DUP n

(* n = size of preserved head of stack *)
let dip n exprs =
  DIP (n, seq exprs)

let push ty cst = PUSH (ty, cst)

(* n = size of preserved head of stack *)
let drop_stack n depth =
  if depth = 0 then [] else
    let rec drop_stack depth =
      if depth = 0 then [] else
        DROP :: (drop_stack (depth-1))
    in
    let exps = drop_stack depth in
    if n = 0 then exps else [DIP_DROP (n, List.length exps)]

(* The type of a contract code is usually:
     lambda (pair (pair tez 'arg) 'global) -> (pair 'ret 'global) *)
let translate_code code =

  let rec compile depth env e =
    match e.desc with
    | Var (name, loc, []) ->
       let pos = try
           StringMap.find name env
         with Not_found ->
           LiquidLoc.raise_error ~loc
                                 "Internal Error(Michelson): variable %S not found\n%!"
                                 name
       in
       [ dup (depth - pos) ], false
    | Var (name, loc, _::_) ->  assert false
    | SetVar (name, loc, _, _) ->  assert false
    | Const (ty, cst) ->
       [ push ty cst ], false
    | Seq (e1, e2) ->
       let e1, transfer1 = compile depth env e1 in
       let (depth, env) =
         if transfer1 then (0, StringMap.empty) else (depth, env)
       in
       let e2, transfer2 = compile depth env e2 in
       e1 @ [  DROP ] @ e2, transfer1 || transfer2

    | Let (name, loc, e1, e2) ->
       let e1, transfer1 = compile depth env e1 in
       let (depth, env) =
         if transfer1 then (0, StringMap.empty) else (depth, env)
       in
       let env = StringMap.add name depth env in
       let depth = depth + 1 in
       let e2, transfer2 = compile depth env e2 in
       let cleanup_stack =
         if transfer2 then [] else [ DIP_DROP (1,1)]
       in
       e1 @ e2 @ cleanup_stack, transfer1 || transfer2

    | Lambda (arg_name, arg_type, loc, body, res_type) ->
       let env = StringMap.empty in
       let env = StringMap.add arg_name 0 env in
       let depth = 1 in
       let body = compile_no_transfer depth env body in
       [ LAMBDA (arg_type, res_type, seq (body @ [DIP_DROP (1,1)])) ], false

    | If (cond, ifthen, ifelse) ->
       let cond = compile_no_transfer depth env cond in
       let (ifthen, transfer1) = compile depth env ifthen in
       let (ifelse, transfer2) = compile depth env ifelse in
       let (ifthen_end, ifelse_end) =
         match transfer1, transfer2 with
         | false, false -> [], []
         | true, true -> [], []
         (* We need to empty the stack before returning, keeping only
             the last value *)
         | true, false -> [], drop_stack 1 depth
         | false, true -> drop_stack 1 depth, []
       in
       cond @ [  (IF (seq (ifthen @ ifthen_end),
                      seq (ifelse @ ifelse_end) ))],
       transfer1 || transfer2

    | LetTransfer( storage_name, result_name,
                   _loc,
                   contract_exp, tez_exp,
                   storage_exp, arg_exp,
                   body_exp) ->
       let storage = compile_no_transfer depth env storage_exp in
       let contract = compile_no_transfer (depth+1) env contract_exp in
       let amount = compile_no_transfer (depth+2) env tez_exp in
       let arg = compile_no_transfer (depth+3) env arg_exp in
       let drop = drop_stack 4 depth in
       let env = StringMap.empty in
       let env = StringMap.add storage_name 0 env in
       let env = StringMap.add result_name 1 env in
       let body, transfer = compile 2 env body_exp in
       let cleanup_stack =
         if transfer then []
         else [ DIP_DROP (1,2) ]
       in
       storage @ contract @ amount @ arg @ drop @
         [  TRANSFER_TOKENS ] @ body @
           cleanup_stack, true

    | Apply (Prim_unknown, _loc, args) -> assert false
    | Apply (prim, _loc, args) ->
       compile_prim depth env prim args, false

    | MatchOption(arg, loc, ifnone, name, ifsome) ->
       let arg, transfer1 = compile depth env arg in
       let (depth, env) =
         if transfer1 then (0, StringMap.empty) else (depth, env)
       in
       let ifnone, transfer2 = compile depth env ifnone in
       let env = StringMap.add name depth env in
       let depth = depth + 1 in
       let ifsome, transfer3 = compile depth env ifsome in
       let (ifnone_end, ifsome_end) =
         match transfer1, transfer2 with
         | false, false -> [], [ DIP_DROP(1,1) ]
         | true, true -> [], []
         | true, false -> [], drop_stack 1 depth
         | false, true -> drop_stack 1 (depth-1), []
       in
       arg @ [  (IF_NONE (seq (ifnone @ ifnone_end),
                          seq (ifsome @ ifsome_end) ))],
       transfer1 || transfer2 || transfer3

    | MatchList(arg, loc, head_name, tail_name, ifcons, ifnil) ->
       let arg, transfer1 = compile depth env arg in
       let (depth, env) =
         if transfer1 then (0, StringMap.empty) else (depth, env)
       in
       let ifnil, transfer2 = compile depth env ifnil in
       let env = StringMap.add tail_name depth env in
       let env = StringMap.add head_name (depth+1) env in
       let depth = depth + 2 in
       let ifcons, transfer3 = compile depth env ifcons in
       let (ifnil_end, ifcons_end) =
         match transfer1, transfer2 with
         | false, false -> [], [ DIP_DROP(1,2) ]
         | true, true -> [], []
         | true, false -> [], drop_stack 1 depth
         | false, true -> drop_stack 1 (depth-2), []
       in
       arg @ [  (IF_CONS (seq (ifcons @ ifcons_end),
                          seq (ifnil @ ifnil_end) ))],
       transfer1 || transfer2 || transfer3

    | MatchVariant(arg, loc, cases) ->
       let arg, transfer1 = compile depth env arg in
       let (depth, env) =
         if transfer1 then (0, StringMap.empty) else (depth, env)
       in
       let has_transfer = ref false in
       let cases = List.map (function
                             | (constr, [ arg_name ], e) ->
                                let env = StringMap.add arg_name depth env in
                                let depth = depth + 1 in
                                let e, transfer = compile depth env e in
                                if transfer then has_transfer := true;
                                e, transfer, depth
                             | _ -> assert false) cases in

       let rec iter cases =
         match cases with
         | [] -> assert false
         | (left, transfer, depth) :: cases ->
            let left_end =
              match !has_transfer, transfer with
              | true, true -> []
              | false, false -> [ DIP_DROP(1,1) ]
              | false, true -> assert false
              | true, false -> drop_stack 1 (depth-1)
            in
            let left = left @ left_end in
            match cases with
            | [] -> left
            | _ ->
               let right = iter cases in
               [ IF_LEFT( seq left, seq right ) ]
       in
       arg @ iter cases, !has_transfer

    | Loop (name, _loc, body, arg) ->
       let arg, transfer1 = compile depth env arg in
       let (depth, env) =
         if transfer1 then (0, StringMap.empty) else (depth, env)
       in
       let env = StringMap.add name depth env in
       let depth = depth + 1 in
       let body, transfer2 = compile depth env body in
       let body_end = [ DIP_DROP (1,1); DUP 1; CAR; DIP (1, seq [ CDR ]) ] in
       arg
       @ [ PUSH (Tbool, CBool true)]
       @ [ LOOP (seq (body @ body_end)) ],
       transfer1 || transfer2

    (* removed during typechecking, replaced by tuple *)
    | Record _ -> assert false
    | Constructor _ -> assert false

  and compile_prim depth env prim args =
    match prim, args with
    | Prim_tuple, args ->
       compile_tuple depth env (List.rev args)

    | Prim_tuple_get, [arg; { desc = Const (_, (CInt n | CNat n))} ] ->
       let arg = compile_no_transfer depth env arg in
       let n = int_of_string n in
       arg @ [  CDAR n ]
    | Prim_tuple_get, _ -> assert false

    | Prim_tuple_get_last, [arg; { desc = Const (_, (CInt n | CNat n))} ] ->
       let arg = compile_no_transfer depth env arg in
       let n = int_of_string n in
       arg @ [  CDDR (n-1) ]
    | Prim_tuple_get_last, _ -> assert false

    (*
set x n y = x + [ DUP; CAR; SWAP; CDR ]*n +
                [ CDR ] + y + [ PAIR ] +
                [ SWAP; PAIR ]*2
     *)

    | Prim_tuple_set, [x; { desc = Const (_, (CInt n | CNat n))}; y ] ->
       let x_code = compile_no_transfer depth env x in
       let n = int_of_string n in
       let set_code = compile_prim_set false (depth+1) env n y in
       x_code @ set_code
    | Prim_tuple_set, _ -> assert false

    | Prim_tuple_set_last, [x; { desc = Const (_, (CInt n | CNat n))}; y ] ->
       let x_code = compile_no_transfer depth env x in
       let n = int_of_string n in
       let set_code = compile_prim_set true (depth+1) env n y in
       x_code @ set_code
    | Prim_tuple_set_last, _ -> assert false

    | Prim_fail,_ -> [ FAIL ]
    | Prim_self, _ -> [ SELF ]
    | Prim_balance, _ -> [ BALANCE ]
    | Prim_now, _ -> [ NOW ]
    | Prim_amount, _ -> [ AMOUNT ]
    | Prim_gas, _ -> [ STEPS_TO_QUOTA ]

    | Prim_Left, [ arg; { ty = right_ty }] ->
       compile_no_transfer depth env arg @
         [ LEFT right_ty]
    | Prim_Left, _ -> assert false

    | Prim_Right, [ arg; { ty = left_ty } ] ->
       compile_no_transfer depth env arg @
         [ RIGHT left_ty ]
    | Prim_Right, _ -> assert false

    | Prim_Source, [ { ty = from_ty }; { ty = to_ty } ] ->
       [ SOURCE (from_ty, to_ty) ]
    | Prim_Source, _ -> assert false

    (* catch the special case of [a;b;c] where
the ending NIL is not annotated with a type *)
    | Prim_Cons, [ { ty } as arg; { ty = Tunit } ] ->
       let arg = compile_no_transfer (depth+1) env arg in
       [ PUSH (Tlist ty, CList[]) ] @ arg @ [ CONS ]

    (* Should be removed in LiquidCheck *)
    | Prim_unknown, _ -> assert false

    (* Should have disappeared *)
    | Prim_unused, _ -> assert false
    | (Prim_coll_find|Prim_coll_update|Prim_coll_mem|Prim_coll_reduce|
       Prim_coll_map|Prim_coll_size), _ -> assert false

    | (Prim_apply|Prim_eq|Prim_neq|Prim_lt|Prim_le|Prim_gt|Prim_ge
       | Prim_compare|Prim_add|Prim_sub|Prim_mul|Prim_ediv|Prim_map_find
       | Prim_map_update|Prim_map_mem|Prim_map_reduce|Prim_map_map
       | Prim_set_update|Prim_set_mem|Prim_set_reduce|Prim_Some
       | Prim_concat|Prim_list_reduce|Prim_list_map|Prim_manager
       | Prim_create_account|Prim_create_contract|Prim_set_map
       | Prim_hash|Prim_check|Prim_default_account|Prim_list_size
       | Prim_set_size|Prim_map_size|Prim_or|Prim_and|Prim_xor
       | Prim_not|Prim_abs|Prim_int|Prim_neg|Prim_lsr|Prim_lsl
       | Prim_exec|Prim_Cons),_ ->
       let _depth, args_code = compile_args depth env args in
       let prim_code = match prim, List.length args with
         | Prim_eq, 2 -> [  COMPARE;  EQ ]
         | Prim_neq, 2 -> [  COMPARE;  NEQ ]
         | Prim_lt, 2 -> [  COMPARE;  LT ]
         | Prim_le, 2 -> [  COMPARE;  LE ]
         | Prim_gt, 2 -> [  COMPARE;  GT ]
         | Prim_ge, 2 -> [ COMPARE; GE ]
         | Prim_compare, 2 -> [  COMPARE ]
         | Prim_add, 2 -> [ ADD ]
         | Prim_sub, 2 -> [ SUB ]
         | Prim_mul, 2 -> [ MUL ]
         | Prim_ediv, 2 -> [ EDIV ]
         | Prim_map_find, 2 -> [ GET ]
         | Prim_map_update, 3 -> [ UPDATE ]
         | Prim_map_mem, 2 -> [ MEM ]
         | Prim_map_reduce, 3 -> [ REDUCE ]
         | Prim_map_map, 2 -> [ MAP ]

         | Prim_set_update, 3 -> [ UPDATE ]
         | Prim_set_mem, 2 -> [ MEM ]
         | Prim_set_reduce, 3 -> [ REDUCE ]
         | Prim_set_map, 2 -> [ MAP ]

         | Prim_Some, 1 -> [ SOME ]
         | Prim_concat, 2 -> [ CONCAT ]

         | Prim_list_reduce, 3 -> [ REDUCE ]
         | Prim_list_map, 2 -> [ MAP ]

         | Prim_manager, 1 -> [ MANAGER ]
         | Prim_create_account, 4 -> [ CREATE_ACCOUNT ]
         | Prim_create_contract, 7 -> [ CREATE_CONTRACT ]
         | Prim_hash, 1 -> [ H ]
         | Prim_check, 2 -> [ CHECK_SIGNATURE ]
         | Prim_default_account, 1 -> [ DEFAULT_ACCOUNT ]
         | Prim_list_size, 1 -> [ SIZE ]
         | Prim_set_size, 1 -> [ SIZE ]
         | Prim_map_size, 1 -> [ SIZE ]

         | Prim_Cons, 2 -> [ CONS ]
         | Prim_or, 2 -> [ OR ]
         | Prim_and, 2 -> [ AND ]
         | Prim_xor, 2 -> [ XOR ]
         | Prim_not, 1 -> [ NOT ]
         | Prim_abs, 1 -> [ ABS ]
         | Prim_int, 1 -> [ INT ]
         | Prim_neg, 1 -> [ NEG ]
         | Prim_lsr, 2 -> [ LSR ]
         | Prim_lsl, 2 -> [ LSL ]

         | Prim_exec, 2 -> [ EXEC ]

         | (Prim_apply|Prim_eq|Prim_neq|Prim_lt|Prim_le|Prim_gt|Prim_ge
            | Prim_compare|Prim_add|Prim_sub|Prim_mul|Prim_ediv|Prim_map_find
            | Prim_map_update|Prim_map_mem|Prim_map_reduce|Prim_map_map
            | Prim_set_update|Prim_set_mem|Prim_set_reduce|Prim_Some
            | Prim_concat|Prim_list_reduce|Prim_list_map|Prim_manager
            | Prim_create_account|Prim_create_contract
            | Prim_hash|Prim_check|Prim_default_account|Prim_list_size
            | Prim_set_size|Prim_map_size|Prim_or|Prim_and|Prim_xor
            | Prim_not|Prim_abs|Prim_int|Prim_neg|Prim_lsr|Prim_lsl
            | Prim_exec|Prim_Cons|Prim_set_map),n ->
            Printf.eprintf "Primitive %S: wrong number of args(%d)\n%!"
                           (LiquidTypes.string_of_primitive prim)
                           n;
            assert false
         (*                           | prim, args -> *)

         | (Prim_unknown|Prim_tuple_get_last|Prim_tuple_get
            | Prim_tuple_set_last|Prim_tuple_set|Prim_tuple|Prim_fail
            | Prim_self|Prim_balance|Prim_now|Prim_amount|Prim_gas
            | Prim_Left|Prim_Right|Prim_Source|Prim_unused
            | Prim_coll_find|Prim_coll_update|Prim_coll_mem
            | Prim_coll_reduce|Prim_coll_map|Prim_coll_size), _ ->
            (* already filtered out *)
            assert false

       in
       args_code @ prim_code


  and compile_prim_set last depth env n y =
    if n = 0 then
      if last then
        [ DROP ]
        @ compile_no_transfer (depth-1) env y
      else
        [ CDR ] @ compile_no_transfer depth env y @ [ PAIR ]
    else
      [ DUP 1; CAR; SWAP; CDR ] @
        compile_prim_set last (depth+1) env (n-1) y @
          [ SWAP; PAIR ]

  and compile_no_transfer depth env e =
    let (e, transfer) = compile depth env e in
    assert (not transfer);
    e

  and compile_args depth env args =
    match args with
    | [] -> depth,[]
    | arg :: args ->
       let (depth, args) = compile_args depth env args in
       let arg = compile_no_transfer depth env arg in
       depth+1, args @ arg

  and compile_tuple depth env args =
    match args with
    | []  -> assert false
    | [_] -> assert false
    | arg :: args ->
       let arg = compile_no_transfer depth env arg in
       let args = compile_tuple1 depth env args in
       arg @ args

  and compile_tuple1 depth env args =
    match args with
    | [] -> []
    | arg :: args ->
       let arg = compile_no_transfer (depth+1) env arg in
       let args = compile_tuple1 depth env args in
       arg @ [ PAIR ] @ args

  in

  let env = StringMap.empty in
  let env = StringMap.add "storage/1" 0 env in
  let env = StringMap.add "parameter/2" 1 env in
  let depth = 2 in

  let exprs, transfer = compile depth env code in

  (* replace ( parameter, storage ) *)
  let header = [
      dup 1;
      dip 1 [ CDR ];
      CAR;
    ]
  in
  let trailer = if transfer then [] else drop_stack 1 depth in
  seq (header @ exprs @ trailer)

(*
let translate filename ~peephole contract =
  let pre_code = translate_code contract.code in
  begin
    let pre_optim_code = LiquidEmit.emit pre_code in
    let s =
      LiquidPrinter.Michelson.string_of_contract
        { contract with code = pre_optim_code } in
    FileString.write_file (filename ^ ".mic") s
  end;
  let pre_code =
    if peephole then
      LiquidPeephole.simplify_pre pre_code
    else pre_code in
  let code = LiquidEmit.emit pre_code in
  { contract with code }
 *)
let translate contract =
  { contract with code = translate_code contract.code }
