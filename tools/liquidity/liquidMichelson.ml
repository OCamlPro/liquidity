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

let loc_of_many (l : loc_michelson list) = match l, List.rev l with
  | [], _ | _, [] -> LiquidLoc.noloc
  | first :: _, last :: _ -> LiquidLoc.merge first.loc last.loc

let ii ~loc ins = { ins; loc; loc_name = None }

let seq exprs = ii ~loc:(loc_of_many exprs) (SEQ exprs)

let dup ~loc n = ii ~loc (DUP n)

(* n = size of preserved head of stack *)
let dip ~loc n exprs = ii ~loc (DIP (n, seq exprs))

let push ~loc ty cst = ii ~loc (PUSH (LiquidEncode.encode_type ty, cst))

let sanitize_name s =
  let to_change = ref [] in
  let sharp_s = "_sharp_" in
  let slash_s = "_slash_" in
  let prim_s = "_prim_" in
  String.iteri (fun i -> function
      | '#' -> to_change := (i, sharp_s) :: !to_change
      | '/' -> to_change := (i, slash_s) :: !to_change
      | '\'' -> to_change := (i, prim_s) :: !to_change
      | _ -> ()
    ) s;
  match !to_change with
  | [] -> s
  | to_change ->
    let new_s = ref "" in
    let last = ref 0 in
    List.iter(fun (i, repl) ->
        new_s := !new_s ^ String.sub s !last (i - !last) ^ repl;
        last := i + 1;
      ) (List.rev to_change);
    let len = String.length s in
    if !last >= len then !new_s
    else !new_s ^ String.sub s !last (len - !last)

let sanitize_opt = function
  | Some s -> Some (sanitize_name s)
  | None -> None

let encode_failwith_param s =
  let codes = Array.make (String.length s) "" in
  String.iteri (fun i c ->
      codes.(i) <- Printf.sprintf "%02x" (Char.code c)
    ) s;
  String.concat "" ("x" :: Array.to_list codes)

(* n = size of preserved head of stack *)
let drop_stack ~loc n depth =
  if depth = 0 then [] else
    let rec drop_stack depth =
      if depth = 0 then [] else
        ii ~loc DROP :: (drop_stack (depth-1))
    in
    let exps = drop_stack depth in
    if n = 0 then exps else [ii ~loc @@ DIP_DROP (n, List.length exps)]

(* The type of a contract code is usually:
     lambda (pair (pair tez 'arg) 'global) -> (pair 'ret 'global) *)
let rec translate_code code =

  let rec compile_desc depth env desc =
    match desc with
    | Var (name, loc, []) ->
       let pos = try
           StringMap.find name env
         with Not_found ->
           LiquidLoc.raise_error ~loc
                                 "Internal Error(Michelson): variable %S not found\n%!"
                                 name
       in
       [ dup ~loc (depth - pos) ]
    | Var (name, loc, _::_) ->  assert false
    | SetVar (name, loc, _, _) ->  assert false
    | Const (loc, ty, cst) ->
       [ push ~loc ty cst ]
    | Seq (e1, e2) ->
       let e1 = compile depth env e1 in
       let e2 = compile depth env e2 in
       e1 @ [ ii ~loc:LiquidLoc.noloc DROP ] @ e2

    | Let (name, loc, e1, e2) ->
       let e1 = compile depth env e1 in
       let env = StringMap.add name depth env in
       let depth = depth + 1 in
       let e2 = compile depth env e2 in
       let cleanup_stack = [ ii ~loc @@ DIP_DROP (1, 1) ] in
       e1 @ e2 @ cleanup_stack

    | Lambda (arg_name, arg_type, loc, body, res_type) ->
       let env = StringMap.empty in
       let env = StringMap.add arg_name 0 env in
       let depth = 1 in
       let arg_type = LiquidEncode.encode_type arg_type in
       let res_type = LiquidEncode.encode_type res_type in
       let body = compile depth env body in
       let arg_annot = compile_arg_name arg_name in
       [ ii ~loc @@
         LAMBDA (arg_type, res_type,
                 seq (arg_annot @ body @ [ii ~loc @@ DIP_DROP (1,1)])) ]

    | Closure (arg_name, p_arg_type, loc, call_env, body, res_type) ->
      let call_env_code = match call_env with
        | [] -> assert false
        | [_, e] -> compile depth env e
        | _ -> compile_tuple ~loc depth env (List.rev_map snd call_env)
      in
      let p_arg_type = LiquidEncode.encode_type p_arg_type in
      let res_type = LiquidEncode.encode_type res_type in
      call_env_code @
      compile_desc depth env
        (Lambda (arg_name, p_arg_type, loc, body, res_type)) @
      [ ii ~loc PAIR ]

    | If (cond, ifthen, ifelse) ->
      let cond = compile depth env cond in
      let ifthen = compile depth env ifthen in
      let ifelse = compile depth env ifelse in
      let loc = loc_of_many cond in
      cond @ [ ii ~loc @@ IF (seq ifthen, seq ifelse)]

    | Transfer(loc, contract_exp, tez_exp, arg_exp) ->
       let contract = compile depth env contract_exp in
       let amount = compile (depth+1) env tez_exp in
       let arg = compile (depth+2) env arg_exp in
       contract @ amount @ arg @ [ ii ~loc TRANSFER_TOKENS ]

    | Failwith (s, loc) ->
      let ins = ii ~loc (FAIL (Some s)) in
      let s = encode_failwith_param s in
      [ ii ~loc (RENAME (Some s)); ins ] (* FAIL must be in tail position *)

    | Apply (Prim_unknown, _loc, args) -> assert false

    | Apply (Prim_exec, loc, [arg; { ty = Tclosure _ } as f]) ->
      let f_env = compile depth env f in
      let arg = compile (depth+1) env arg in
      f_env @ arg @
      [ dip ~loc 1 [ dup ~loc 1; ii ~loc CAR; ii ~loc SWAP; ii ~loc CDR] ] @
      [ ii ~loc PAIR ; ii ~loc EXEC ]

    | Apply (prim, loc, ([_; { ty = Tlambda _ } ] as args)) ->
      compile_prim ~loc depth env prim args

    | Apply (prim, loc, args) ->
       compile_prim ~loc depth env prim args

    | MatchOption(arg, loc, ifnone, name, ifsome) ->
       let arg = compile depth env arg in
       let ifnone = compile depth env ifnone in
       let env = StringMap.add name depth env in
       let depth = depth + 1 in
       let ifsome = compile depth env ifsome in
       let loc2, loc3 = loc_of_many ifnone, loc_of_many ifsome in
       let ifsome_end = [ii ~loc:loc3 @@ DIP_DROP(1,1)] in
       arg @ [ ii ~loc @@ IF_NONE (seq ifnone, seq (ifsome @ ifsome_end) )]

    | MatchNat(arg, loc, plus_name, ifplus, minus_name, ifminus) ->
       let arg = compile depth env arg in
       let env' = StringMap.add plus_name depth env in
       let ifplus = compile (depth + 1) env' ifplus in
       let env'' = StringMap.add minus_name depth env in
       let ifminus = compile (depth + 1) env'' ifminus in
       let loc2, loc3 = loc_of_many ifplus, loc_of_many ifminus in
       let (ifplus_end, ifminus_end) =
         [ ii ~loc:loc2 @@ DIP_DROP(1,1) ],
         [ ii ~loc:loc3 @@ DIP_DROP(1,1) ] in
       arg @ [
         dup ~loc 1; ii ~loc ABS; ii ~loc SWAP; ii ~loc GE;
         ii ~loc @@ IF (seq (ifplus @ ifplus_end),
                        seq (ifminus @ ifminus_end) )]

    | MatchList(arg, loc, head_name, tail_name, ifcons, ifnil) ->
       let arg = compile depth env arg in
       let ifnil = compile depth env ifnil in
       let env = StringMap.add tail_name depth env in
       let env = StringMap.add head_name (depth+1) env in
       let depth = depth + 2 in
       let ifcons = compile depth env ifcons in
       let loc2, loc3 = loc_of_many ifnil, loc_of_many ifcons in
       let ifcons_end = [ii ~loc:loc3 @@ DIP_DROP(1,2)] in
       arg @ [ ii ~loc @@ IF_CONS (seq (ifcons @ ifcons_end), seq ifnil )]

    | MatchVariant(arg, loc, cases) ->
       let arg = compile depth env arg in
       let cases = List.map (function
           | CConstr (constr, [ arg_name ]), e ->
             let env = StringMap.add arg_name depth env in
             let depth = depth + 1 in
             let e = compile depth env e in
             arg_name, e, depth
           | _ -> assert false) cases
       in
       let rec iter cases =
         match cases with
         | [] -> assert false
         | (arg_name, left, depth) :: cases ->
            let left_end = [ii ~loc @@ DIP_DROP(1,1)] in
            (* let arg_annot = compile_arg_name arg_name in *)
            let left = (* arg_annot @ *) left @ left_end in
            match cases with
            | [] -> left
            | _ ->
               let right = iter cases in
               [ii ~loc @@ IF_LEFT( seq (left), seq right )]
       in
       arg @ iter cases

    | Loop (name, loc, body, arg) ->
       let arg = compile depth env arg in
       let env = StringMap.add name depth env in
       let depth = depth + 1 in
       let arg_annot = compile_arg_name name in
       let body = compile depth env body in
       let body_end = [ ii ~loc @@ DIP_DROP (1,1);
                        ii ~loc @@ DUP 1;
                        ii ~loc CAR;
                        ii ~loc @@ DIP (1, seq [ ii ~loc CDR ]) ] in
       arg
       @ [ ii ~loc @@ PUSH (Tbool, CBool true) ]
       @ [ ii ~loc @@ LOOP (seq (arg_annot @ body @ body_end)) ]

    | Fold (prim, name, loc, body, arg, acc) ->
      let acc = compile depth env acc in
      let depth = depth + 1 in
      let arg = compile depth env arg in
      let env = StringMap.add name depth env in
      let depth = depth + 1 in
      let arg_annot = compile_arg_name name in
      let body = compile depth env body in
      let body_begin = match prim with
        | Prim_map_iter | Prim_set_iter | Prim_list_iter ->
          []
        | _ ->
          [ dip ~loc 1 [ii ~loc @@ DUP 1]; ii ~loc PAIR ]
      in
      let body_end = [ ii ~loc @@ DIP_DROP (1,2) ] in
      acc @ arg @
      [ii ~loc @@ ITER (seq (arg_annot @ body_begin @ body @ body_end))]

    | Map (_prim, name, loc, body, arg) ->
      let arg = compile depth env arg in
      let env = StringMap.add name depth env in
      let depth = depth + 1 in
      let arg_annot = compile_arg_name name in
      let body = compile depth env body in
      let body_end = [ ii ~loc @@ DIP_DROP (1,1) ] in
      arg @
      [ii ~loc @@ MAP (seq (arg_annot @ body @ body_end))]

    | MapFold (_prim, name, loc, body, arg, acc) ->
      let acc = compile depth env acc in
      let depth = depth + 1 in
      let arg = compile depth env arg in
      let env = StringMap.add name depth env in
      let depth = depth + 1 in
      let arg_annot = compile_arg_name name in
      let body = compile depth env body in
      let body_begin = [ dip ~loc 1 [ii ~loc @@ DUP 1]; ii ~loc PAIR ] in
      let body_end = [
        ii ~loc @@ DIP_DROP (1,2);
        dup ~loc 1;
        dip ~loc 1 [ ii ~loc CDR ];
        ii ~loc CAR;
      ] in
      acc @ arg @
      [ii ~loc @@ MAP (seq (arg_annot @ body_begin @ body @ body_end));
       ii ~loc PAIR ]
      (* TODO check this *)

    | CreateContract (loc, args, contract) ->
      let _depth, args_code = compile_args depth env args in
      let contract = translate contract in
      args_code @
      [ii ~loc @@ CREATE_CONTRACT contract; ii ~loc PAIR]

    | ContractAt (loc, addr, ty) ->
      let ty = LiquidEncode.encode_type ty in
      compile depth env addr @
      [ ii ~loc (CONTRACT ty) ]

    (* removed during typechecking, replaced by tuple *)
    | Record _ -> assert false
    | Constructor _ -> assert false

  and compile_prim ~loc depth env prim args =
    let ii = ii ~loc in
    match prim, args with
    | Prim_tuple, args ->
       compile_tuple ~loc depth env (List.rev args)

    | Prim_tuple_get, [arg; { desc = Const (loc, _, (CInt n | CNat n))} ] ->
       let size = size_of_type arg.ty in
       let arg = compile depth env arg in
       let n = LiquidPrinter.int_of_integer n in
       let ins =
         if size = n + 1 then
           ii @@ CDDR (n-1)
         else
           ii @@ CDAR n
       in
       arg @ [ ins ]
    | Prim_tuple_get, _ -> assert false

    (*
set x n y = x + [ DUP; CAR; SWAP; CDR ]*n +
                [ CDR ] + y + [ PAIR ] +
                [ SWAP; PAIR ]*2
     *)

    | Prim_tuple_set, [x; { desc = Const (loc, _, (CInt n | CNat n))}; y ] ->
       let x_code = compile depth env x in
       let n = LiquidPrinter.int_of_integer n in
       let size = size_of_type x.ty in
       let is_last = size = n + 1 in
       let set_code = compile_prim_set ~loc is_last (depth+1) env n y in
       x_code @ set_code
    | Prim_tuple_set, _ -> assert false

    | Prim_fail,_ -> [ ii @@ FAIL None ]

    | Prim_self, _ -> [ ii SELF ]
    | Prim_balance, _ -> [ ii BALANCE ]
    | Prim_now, _ -> [ ii NOW ]
    | Prim_amount, _ -> [ ii AMOUNT ]
    | Prim_gas, _ -> [ ii STEPS_TO_QUOTA ]
    | Prim_source, _ -> [ ii SOURCE ]

    | Prim_Left, [ arg; { ty = right_ty }] ->
      let right_ty = LiquidEncode.encode_type right_ty in
      compile depth env arg @
      [ ii (LEFT right_ty) ]
    | Prim_Left, _ -> assert false

    | Prim_Right, [ arg; { ty = left_ty } ] ->
      let left_ty = LiquidEncode.encode_type left_ty in
      compile depth env arg @
      [ ii (RIGHT left_ty) ]
    | Prim_Right, _ -> assert false

    (* catch the special case of [a;b;c] where
the ending NIL is not annotated with a type *)
    | Prim_Cons, [ { ty } as arg; { ty = Tunit } ] ->
      let ty = LiquidEncode.encode_type ty in
      let arg = compile (depth+1) env arg in
      [ push ~loc (Tlist ty) (CList[]) ] @ arg @ [ ii CONS ]

    (* Should be removed in LiquidCheck *)
    | Prim_unknown, _
    | Prim_list_rev, _ -> assert false

    (* Should have disappeared *)
    | Prim_unused, _ -> assert false
    | (Prim_coll_find|Prim_coll_update|Prim_coll_mem|Prim_coll_size), _ ->
      assert false

    | ( Prim_eq|Prim_neq|Prim_lt|Prim_le|Prim_gt|Prim_ge
      | Prim_compare|Prim_add|Prim_sub|Prim_mul|Prim_ediv|Prim_map_find
      | Prim_map_update|Prim_map_add|Prim_map_remove
      | Prim_map_mem
      | Prim_set_update|Prim_set_add|Prim_set_remove
      | Prim_set_mem|Prim_Some
      | Prim_concat|Prim_manager
      | Prim_create_account
      | Prim_hash|Prim_hash_key|Prim_check|Prim_default_account|Prim_list_size
      | Prim_set_size|Prim_map_size|Prim_or|Prim_and|Prim_xor
      | Prim_not|Prim_abs|Prim_int|Prim_neg|Prim_lsr|Prim_lsl
      | Prim_exec|Prim_Cons|Prim_set_delegate|Prim_address),_ ->
      let _depth, args_code = compile_args depth env args in
       let prim_code = match prim, List.length args with
         | Prim_eq, 2 -> [ ii COMPARE; ii EQ ]
         | Prim_neq, 2 -> [ ii COMPARE; ii NEQ ]
         | Prim_lt, 2 -> [ ii COMPARE; ii LT ]
         | Prim_le, 2 -> [ ii COMPARE; ii LE ]
         | Prim_gt, 2 -> [ ii COMPARE; ii GT ]
         | Prim_ge, 2 -> [ ii COMPARE; ii GE ]
         | Prim_compare, 2 -> [ ii COMPARE ]
         | Prim_add, 2 -> [ ii ADD ]
         | Prim_sub, 2 -> [ ii SUB ]
         | Prim_mul, 2 -> [ ii MUL ]
         | Prim_ediv, 2 -> [ ii EDIV ]
         | Prim_map_find, 2 -> [ ii GET ]
         | Prim_map_update, 3 -> [ ii UPDATE ]
         | Prim_map_add, 3 -> [dip ~loc 1 [ii SOME]; ii UPDATE ]
         | Prim_map_remove, 2 ->
           let ty = match args with
             | [_; { ty = (Tmap (_, ty) | Tbigmap (_, ty)) }] -> ty
             | _ -> assert false
           in
           [dip ~loc 1 [push ~loc (Toption ty) CNone]; ii UPDATE ]
         | Prim_map_mem, 2 -> [ ii MEM ]

         | Prim_set_update, 3 -> [ ii UPDATE ]
         | Prim_set_add, 2 -> [dip ~loc 1 [push ~loc Tbool (CBool true)]; ii UPDATE ]
         | Prim_set_remove, 2 -> [dip ~loc 1 [push ~loc Tbool (CBool false)]; ii UPDATE ]
         | Prim_set_mem, 2 -> [ ii MEM ]

         | Prim_Some, 1 -> [ ii SOME ]
         | Prim_concat, 2 -> [ ii CONCAT ]

         | Prim_manager, 1 -> [ ii MANAGER ]
         | Prim_address, 1 -> [ ii ADDRESS ]
         | Prim_create_account, 4 -> [ ii CREATE_ACCOUNT; ii PAIR ]
         | Prim_hash, 1 -> [ ii H ]
         | Prim_hash_key, 1 -> [ ii HASH_KEY ]
         | Prim_check, 3 -> [ ii CHECK_SIGNATURE ]
         | Prim_default_account, 1 -> [ ii IMPLICIT_ACCOUNT ]
         | Prim_set_delegate, 1 -> [ ii SET_DELEGATE ]
         | Prim_list_size, 1 -> [ ii SIZE ]
         | Prim_set_size, 1 -> [ ii SIZE ]
         | Prim_map_size, 1 -> [ ii SIZE ]

         | Prim_Cons, 2 -> [ ii CONS ]
         | Prim_or, 2 -> [ ii OR ]
         | Prim_and, 2 -> [ ii AND ]
         | Prim_xor, 2 -> [ ii XOR ]
         | Prim_not, 1 -> [ ii NOT ]
         | Prim_abs, 1 -> [ ii ABS; ii INT ]
         | Prim_int, 1 -> [ ii INT ]
         | Prim_neg, 1 -> [ ii NEG ]
         | Prim_lsr, 2 -> [ ii LSR ]
         | Prim_lsl, 2 -> [ ii LSL ]

         | Prim_exec, 2 -> [ ii EXEC ]

         | (Prim_eq|Prim_neq|Prim_lt|Prim_le|Prim_gt|Prim_ge
           | Prim_compare|Prim_add|Prim_sub|Prim_mul|Prim_ediv|Prim_map_find
           | Prim_map_update|Prim_map_add|Prim_map_remove
           | Prim_map_mem
           | Prim_set_update|Prim_set_add|Prim_set_remove
           | Prim_set_mem|Prim_Some
           | Prim_concat|Prim_manager
           | Prim_create_account
           | Prim_hash|Prim_hash_key|Prim_check|Prim_default_account|Prim_list_size
           | Prim_set_size|Prim_map_size|Prim_or|Prim_and|Prim_xor
           | Prim_not|Prim_abs|Prim_int|Prim_neg|Prim_lsr|Prim_lsl
           | Prim_exec|Prim_Cons|Prim_set_delegate|Prim_address),n ->
           Printf.eprintf "Primitive %S: wrong number of args(%d)\n%!"
             (LiquidTypes.string_of_primitive prim)
             n;
           assert false
         (*                           | prim, args -> *)

         | (Prim_unknown|Prim_tuple_get
           | Prim_tuple_set|Prim_tuple|Prim_fail
           | Prim_self|Prim_balance|Prim_now|Prim_amount|Prim_gas
           | Prim_Left|Prim_Right|Prim_source|Prim_unused
           | Prim_coll_find|Prim_coll_update|Prim_coll_mem
           | Prim_coll_size|Prim_list_rev), _ ->
           (* already filtered out *)
           Printf.eprintf "Primitive %S ?\n%!"
             (LiquidTypes.string_of_primitive prim)
             ;
           assert false

       in
       args_code @ prim_code


  and compile_prim_set ~loc last depth env n y =
    let ii = ii ~loc in
    if n = 0 then
      if last then
        [ ii DROP ]
        @ compile (depth-1) env y
      else
        [ ii CDR ] @ compile depth env y @ [ ii PAIR ]
    else
      [ ii (DUP 1); ii CAR; ii SWAP; ii CDR ] @
        compile_prim_set last ~loc (depth+1) env (n-1) y @
          [ ii SWAP; ii PAIR ]

  and compile_args depth env args =
    match args with
    | [] -> depth,[]
    | arg :: args ->
       let (depth, args) = compile_args depth env args in
       let arg = compile depth env arg in
       depth+1, args @ arg

  and compile_tuple ~loc depth env args =
    match args with
    | []  -> assert false
    | [_] -> assert false
    | arg :: args ->
       let arg = compile depth env arg in
       let args = compile_tuple1 ~loc depth env args in
       arg @ args

  and compile_tuple1 ~loc depth env args =
    match args with
    | [] -> []
    | arg :: args ->
       let arg = compile (depth+1) env arg in
       let args = compile_tuple1 ~loc depth env args in
       arg @ [ ii ~loc PAIR ] @ args

  and compile depth env e =
    let code = compile_desc depth env e.desc in
    compile_name e.name code

  and compile_name name code =
    if not !LiquidOptions.annotmic then code
    else
    if !LiquidOptions.annotafter then
      match name with
      | Some name ->
        code @ [ii ~loc:LiquidLoc.noloc (RENAME (Some (sanitize_name name)))]
      | None -> code
    else
      match List.rev code with
      | c :: _ when name <> None ->
        c.loc_name <- sanitize_opt name;
        code
      | _ -> code

  and compile_arg_name arg_name =
    if !LiquidOptions.annotmic
    then [ii ~loc:LiquidLoc.noloc (RENAME (Some (sanitize_name arg_name)))]
    else []

  in

  let env = StringMap.empty in
  let env = StringMap.add "storage/1" 0 env in
  let env = StringMap.add "parameter/2" 1 env in
  let depth = 2 in

  let exprs = compile depth env code in
  let loc = LiquidLoc.noloc in

  (* replace ( parameter, storage ) *)
  let header = [
      dup ~loc 1;
      dip ~loc 1 [ ii ~loc CDR ];
      ii ~loc CAR;
    ]
  in
  let trailer = drop_stack ~loc 1 depth in
  seq (header @ exprs @ trailer)


and finalize_fail_pre ({ ins } as e) =
  { e with
    ins =
      match ins with
      | SEQ expr -> SEQ (finalize_fail_seq [] expr)
      | IF (e1, e2) -> IF (finalize_fail_pre e1, finalize_fail_pre e2)
      | IF_NONE (e1, e2) -> IF_NONE (finalize_fail_pre e1, finalize_fail_pre e2)
      | IF_LEFT (e1, e2) -> IF_LEFT (finalize_fail_pre e1, finalize_fail_pre e2)
      | IF_CONS (e1, e2) -> IF_CONS (finalize_fail_pre e1, finalize_fail_pre e2)
      | DIP (n, e) -> DIP (n, finalize_fail_pre e)
      | LOOP e -> LOOP (finalize_fail_pre e)
      | ITER e -> ITER (finalize_fail_pre e)
      | LAMBDA (arg_type, res_type, e) ->
        LAMBDA (arg_type, res_type, finalize_fail_pre e)
      | _ -> ins
  }

and finalize_fail_seq acc exprs =
  match exprs with
  | [] -> List.rev acc
  | e :: exprs ->
    let e = finalize_fail_pre e in
    match e.ins with
    | FAIL _ -> List.rev (e :: acc)
    | _ ->
      finalize_fail_seq (e :: acc) exprs

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
and translate contract =
  { contract with
    code = translate_code contract.code |> finalize_fail_pre }
