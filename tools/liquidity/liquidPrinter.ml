(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes




let to_string bprinter x =
  let b = Buffer.create 10_000 in
  let indent = "  " in
  bprinter b indent x;
  Buffer.contents b





module Michelson = struct

(* For now, we always use the multi-line notation, and never output
  parenthesized expressions such as "(contract unit unit)" *)
  let rec bprint_type b indent ty =
    match ty with
    | Tfail -> Printf.bprintf b "failure"
    | Tunit -> Printf.bprintf b "unit"
    | Tbool -> Printf.bprintf b "bool"
    | Tint -> Printf.bprintf b "int"
    | Tnat -> Printf.bprintf b "nat"
    | Ttez -> Printf.bprintf b "tez"
    | Tstring -> Printf.bprintf b "string"
    | Ttimestamp  -> Printf.bprintf b "timestamp"
    | Tkey  -> Printf.bprintf b "key"
    | Tsignature  -> Printf.bprintf b "signature"
    | Ttuple tys -> bprint_type_pairs b indent tys
    | Tcontract (ty1, ty2) ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(contract\n%s" indent;
       bprint_type b indent ty1;
       Printf.bprintf b "\n%s" indent;
       bprint_type b indent ty2;
       Printf.bprintf b "\n%s)" indent;
    | Tor (ty1, ty2) ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(or\n%s" indent;
       bprint_type b indent ty1;
       Printf.bprintf b "\n%s" indent;
       bprint_type b indent ty2;
       Printf.bprintf b "\n%s)" indent;
    | Toption ty ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(option\n%s" indent;
       bprint_type b indent ty;
       Printf.bprintf b "\n%s)" indent;
    | Tlist ty ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(list\n%s" indent;
       bprint_type b indent ty;
       Printf.bprintf b "\n%s)" indent;
    | Tset ty ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(set\n%s" indent;
       bprint_type b indent ty;
       Printf.bprintf b "\n%s)" indent;
    | Tmap (ty1, ty2) ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(map\n%s" indent;
       bprint_type b indent ty1;
       Printf.bprintf b "\n%s" indent;
       bprint_type b indent ty2;
       Printf.bprintf b "\n%s)" indent;
    | Tlambda (ty1, ty2) ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(lambda\n%s" indent;
       bprint_type b indent ty1;
       Printf.bprintf b "\n%s" indent;
       bprint_type b indent ty2;
       Printf.bprintf b "\n%s)" indent;
    | Tclosure ((ty_arg, ty_env), ty_r) ->
       bprint_type b indent
         (Ttuple [Tlambda (Ttuple [ty_arg; ty_env], ty_r);
                  ty_env ]);
    | Ttype (ty_name, ty) ->
       (*     Printf.bprintf b "%S =\n%s  " ty_name indent; *)
       bprint_type b indent ty



  and bprint_type_pairs b indent tys =
    match tys with
    | [] -> assert false
    | [ty] -> bprint_type b indent ty
    | ty :: tys ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(pair\n%s" indent;
       bprint_type b indent ty;
       Printf.bprintf b "\n%s" indent;
       bprint_type_pairs b indent tys;
       Printf.bprintf b "\n%s)" indent;
       ()

  let rec bprint_const b indent cst =
    match cst with
    | CString s -> Printf.bprintf b "%S" s
    | CKey s -> Printf.bprintf b "%S" s
    | CSignature s -> Printf.bprintf b "%S" s
    | CTez s -> Printf.bprintf b "%S" s
    | CInt n | CNat n -> Printf.bprintf b "%s" n
    | CBool true -> Printf.bprintf b "True"
    | CBool false -> Printf.bprintf b "False"
    | CUnit -> Printf.bprintf b "Unit"
    | CNone -> Printf.bprintf b "None"
                              (*
    | CLeft cst ->
       let indent = indent ^ "  " in
       Printf.bprintf b "Left\n%s" indent;
       bprint_const b indent cst
    | CRight cst ->
       let indent = indent ^ "  " in
       Printf.bprintf b "Right\n%s" indent;
       bprint_const b indent cst
                               *)
    | CSome cst ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(Some\n%s" indent;
       bprint_const b indent cst;
       Printf.bprintf b "\n%s)" indent;
    | CLeft cst ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(Left\n%s" indent;
       bprint_const b indent cst;
       Printf.bprintf b "\n%s)" indent;
    | CRight cst ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(Right\n%s" indent;
       bprint_const b indent cst;
       Printf.bprintf b "\n%s)" indent;
    | CTuple tys -> bprint_const_pairs b indent tys
    | CMap pairs ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(Map";
       List.iter (fun (cst1, cst2) ->
           Printf.bprintf b "\n%s(Item" indent;
           let indent = indent ^ "  " in
           Printf.bprintf b "\n%s" indent;
           bprint_const b indent cst1;
           Printf.bprintf b "\n%s" indent;
           bprint_const b indent cst2;
           Printf.bprintf b "\n%s)" indent;
         ) pairs;
       Printf.bprintf b "\n%s)" indent;
    | CList csts ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(List";
       List.iter (fun cst ->
           Printf.bprintf b "\n%s" indent;
           bprint_const b indent cst;
         ) csts;
       Printf.bprintf b "\n%s)" indent;
    | CSet csts ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(Set";
       List.iter (fun cst ->
           Printf.bprintf b "\n%s" indent;
           bprint_const b indent cst;
         ) csts;
       Printf.bprintf b "\n%s)" indent;

  and bprint_const_pairs b indent tys =
    match tys with
    | [] -> assert false
    | [ty] -> bprint_const b indent ty
    | ty :: tys ->
       let indent = indent ^ "  " in
       Printf.bprintf b "(Pair\n%s" indent;
       bprint_const b indent ty;
       Printf.bprintf b "\n%s" indent;
       bprint_const_pairs b indent tys;
       Printf.bprintf b ")";
       ()

  let rec bprint_code b indent code =
    match code with
    | M_INS ins -> Printf.bprintf b "%s ;" ins
    | M_INS_CST (ins,ty,cst) ->
       let indent = indent ^ "  " in
       Printf.bprintf b "%s\n%s" ins indent;
       bprint_type b indent ty;
       Printf.bprintf b "\n%s" indent;
       bprint_const b indent cst;
       Printf.bprintf b " ;";
    | M_INS_EXP ("SEQ", [], []) ->
       Printf.bprintf b "{}"
    | M_INS_EXP ("SEQ", [], exps) ->
       Printf.bprintf b "{";
       let indent_in = indent ^ "  " in
       List.iter (fun exp ->
           Printf.bprintf b "\n%s" indent_in;
           bprint_code b indent_in exp) exps;
       Printf.bprintf b "\n%s}" indent
    | M_INS_EXP (ins,tys, exps) ->
       let indent = indent ^ "  " in
       Printf.bprintf b "%s" ins;
       List.iter (fun ty ->
           Printf.bprintf b "\n%s" indent;
           bprint_type b indent ty) tys;
       List.iter (fun exp ->
           Printf.bprintf b "\n%s" indent;
           bprint_code b indent exp) exps;
       Printf.bprintf b "\n%s;" indent;
       ()


  let bprint_contract bprint_code b indent contract =
    Printf.bprintf b "parameter\n%s" indent;
    bprint_type b indent contract.parameter;
    Printf.bprintf b ";\n";

    Printf.bprintf b "storage\n%s" indent;
    bprint_type b indent contract.storage;
    Printf.bprintf b ";\n";

    Printf.bprintf b "return\n%s" indent;
    bprint_type b indent contract.return;
    Printf.bprintf b ";\n";

    Printf.bprintf b "code\n  ";
    bprint_code b indent contract.code;
    Printf.bprintf b "\n";
    ()


  let string_of_type = to_string bprint_type
  let string_of_code code = to_string bprint_code code
  let string_of_const = to_string bprint_const
  let string_of_contract cmd = to_string (bprint_contract bprint_code) cmd



end



module Liquid = struct

  let bprint_type b indent ty =
    Michelson.bprint_type b indent ty

  let bprint_const b indent cst =
    Michelson.bprint_const b indent cst

  let rec bprint_code ~debug b indent code =
    if debug && not (StringSet.is_empty code.bv) then begin
        Printf.bprintf b "\n%s(*\n" indent;
        (*        bprint_type b indent code.ty; *)
        Printf.bprintf b "%sbound:" indent;
        StringSet.iter (fun s -> Printf.bprintf b " %s" s) code.bv;
        Printf.bprintf b "\n%s*)" indent;
      end;

    match code.desc with
    | Let (name, _loc, exp, body) ->
       let indent2 = indent ^ "  " in
       Printf.bprintf b "\n%slet %s =" indent name;
       bprint_code ~debug b indent2 exp;
       Printf.bprintf b "\n%sin" indent;
       bprint_code ~debug b indent body
    | Const (ty, cst) ->
       let indent2 = indent ^ "  " in
       Printf.bprintf b "\n%s(const\n%s" indent indent2;
       bprint_type b indent2 ty;
       Printf.bprintf b "\n%s" indent2;
       bprint_const b indent2 cst;
       Printf.bprintf b ")";
    | SetVar (name, _loc, labels, e) ->
       let indent2 = indent ^ "  " in
       Printf.bprintf b "\n%s(%s <-" indent (String.concat "." (name :: labels));
       bprint_code ~debug b indent2 e;
       Printf.bprintf b ")";
    | Var (name, _loc, labels) ->
       Printf.bprintf b "\n%s%s" indent (String.concat "." (name :: labels))
    | Apply (prim, _loc, args) ->
       Printf.bprintf b "\n%s(%s" indent
                      (LiquidTypes.string_of_primitive prim);
       let indent2 = indent ^ "  " in
       List.iter (fun exp ->
           bprint_code ~debug b indent2 exp
         ) args;
       Printf.bprintf b ")"
    | If (cond, ifthen, ifelse) ->
       let indent2 = indent ^ "  " in
       Printf.bprintf b "\n%sif" indent;
       bprint_code ~debug b indent2 cond;
       Printf.bprintf b "\n%sthen" indent;
       bprint_code ~debug b indent2 ifthen;
       Printf.bprintf b "\n%selse" indent;
       bprint_code ~debug b indent2 ifelse;
    | Seq (exp1, exp2) ->
       bprint_code ~debug b indent exp1;
       Printf.bprintf b ";";
       bprint_code ~debug b indent exp2
    | LetTransfer (storage, result, _loc, contract_exp, tez_exp,
                   storage_exp, arg_exp, body) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%slet %s, %s =" indent storage result;
       Printf.bprintf b "\n%sContract.call" indent2;
       bprint_code ~debug b indent4 contract_exp;
       bprint_code ~debug b indent4 tez_exp;
       bprint_code ~debug b indent4 storage_exp;
       bprint_code ~debug b indent4 arg_exp;
       Printf.bprintf b "\n%sin" indent;
       bprint_code ~debug b indent body
    | MatchOption (arg, _loc, ifnone, var, ifsome) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%smatch " indent;
       bprint_code ~debug b indent2 arg;
       Printf.bprintf b " with\n";
       Printf.bprintf b "\n%s| None ->\n" indent2;
       bprint_code ~debug b indent4 ifnone;
       Printf.bprintf b "\n%s| Some %s ->\n" indent2 var;
       bprint_code ~debug b indent4 ifsome;
       ()
    | MatchList (arg, _loc, head_name, tail_name, ifcons, ifnil) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%smatch " indent;
       bprint_code ~debug b indent2 arg;
       Printf.bprintf b " with\n";
       Printf.bprintf b "\n%s| [] ->\n" indent2;
       bprint_code ~debug b indent4 ifnil;
       Printf.bprintf b "\n%s| %s :: %s ->\n" indent2 head_name tail_name;
       bprint_code ~debug b indent4 ifcons;
       ()
    | Loop (name, _loc, body, arg) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%sLoop.loop (fun %s -> " indent name;
       bprint_code ~debug b indent4 body;
       Printf.bprintf b ")\n%s" indent2;
       bprint_code ~debug b indent2 arg;
       ()
    | Closure (arg_name, arg_type, _loc, _, body, res_type)
    (* FIXME change this *)
    | Lambda (arg_name, arg_type, _loc, body, res_type) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%s(fun ( %s : " indent arg_name;
       bprint_type b indent2 arg_type;
       Printf.bprintf b ") ->\n%s" indent2;
       bprint_code ~debug b indent4 body;
       Printf.bprintf b ")"
    | Record (_loc, lab_x_exp_list) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%s{" indent;
       List.iter (fun (label, exp) ->
           Printf.bprintf b "\n%s%s = " indent2 label;
           bprint_code ~debug b indent4 exp;
           Printf.bprintf b ";";
         ) lab_x_exp_list;
       Printf.bprintf b "}"
    | Constructor (_loc, Constr constr, arg) ->
       Printf.bprintf b "\n%s%s (" indent constr;
       bprint_code ~debug b (indent ^ "  ") arg;
       Printf.bprintf b ")"
    | Constructor (_loc, Left right_ty, arg) ->
       Printf.bprintf b "\n%s(Left " indent;
       bprint_code ~debug b (indent ^ "  ") arg;
       Printf.bprintf b " : (_, ";
       bprint_type b (indent ^ "  ") right_ty;
       Printf.bprintf b ") variant)"
    | Constructor (_loc, Right right_ty, arg) ->
       Printf.bprintf b "\n%s(Right " indent;
       bprint_code ~debug b (indent ^ "  ") arg;
       Printf.bprintf b " : ( ";
       bprint_type b (indent ^ "  ") right_ty;
       Printf.bprintf b ", _) variant)"
    | Constructor (_loc, Source (from_ty, to_ty), _arg) ->
       Printf.bprintf b "\n%s(Source " indent;
       Printf.bprintf b " : ( ";
       bprint_type b (indent ^ "  ") from_ty;
       Printf.bprintf b ", ";
       bprint_type b (indent ^ "  ") to_ty;
       Printf.bprintf b ",) contract)"
    | MatchVariant (arg, _loc, cases) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%smatch " indent;
       bprint_code ~debug b indent2 arg;
       Printf.bprintf b " with\n";
       List.iter (fun (constr, vars, e) ->
           Printf.bprintf b "\n%s| %s (%s) ->\n" indent2 constr
                          (String.concat ", " vars);
           bprint_code ~debug b indent4 e;
         ) cases;
       ()


  let bprint_contract ~debug b indent contract =
    let indent2 = indent ^ "    " in
    Printf.bprintf b "let contract\n";
    Printf.bprintf b "    (amount: tez)\n";
    Printf.bprintf b "    (parameter: ";
    bprint_type b indent2 contract.parameter;
    Printf.bprintf b ")\n";
    Printf.bprintf b "    (storage: ";
    bprint_type b indent2 contract.storage;
    Printf.bprintf b ")\n";
    Printf.bprintf b "    (return: ";
    bprint_type b indent2 contract.return;
    Printf.bprintf b ") = \n";

    bprint_code ~debug b indent contract.code

  let string_of_type = to_string bprint_type
  let string_of_const = to_string bprint_const
  let string_of_code ?(debug=false) code =
    to_string (bprint_code ~debug) code
  let string_of_contract ?(debug=false) cmd =
    to_string (bprint_contract ~debug) cmd

end

let string_of_node node =
  match node.kind with
  | N_VAR s -> Printf.sprintf "N_VAR %S" s
  | N_START -> "N_START"
  | N_IF _ -> "N_IF"
  | N_IF_RESULT (_,int) -> Printf.sprintf "N_IF_RESULT %d" int
  | N_IF_THEN _ -> "N_IF_THEN"
  | N_IF_ELSE _ -> "N_IF_ELSE"
  | N_IF_END _ -> "N_IF_END"
  | N_IF_END_RESULT (_, _, int) -> Printf.sprintf "N_IF_END_RESULT %d" int
  | N_IF_NONE _ -> "N_IF_NONE"
  | N_IF_SOME _ -> "N_IF_SOME"
  | N_IF_NIL _ -> "N_IF_NIL"
  | N_IF_CONS _ -> "N_IF_CONS"
  | N_IF_LEFT _ -> "N_IF_LEFT"
  | N_IF_RIGHT _ -> "N_IF_RIGHT"
  | N_TRANSFER _ -> "N_TRANSFER"
  | N_TRANSFER_RESULT int -> Printf.sprintf "N_TRANSFER_RESULT %d" int
  | N_CONST (ty, cst) -> "N_CONST " ^ Michelson.string_of_const cst
  | N_PRIM string ->
     Printf.sprintf "N_PRIM %s" string
  | N_FAIL -> "N_FAIL"
  | N_LOOP _ -> "N_LOOP"
  | N_LOOP_BEGIN _ -> "N_LOOP_BEGIN"
  | N_LOOP_END _ -> "N_LOOP_END"
  | N_LOOP_ARG (_,int) -> Printf.sprintf "N_LOOP_ARG %d" int
  | N_LOOP_RESULT (_,_, int) -> Printf.sprintf "N_LOOP_RESULT %d" int
  | N_LAMBDA _ -> "N_LAMBDA"
  | N_LAMBDA_BEGIN -> "N_LAMBDA_BEGIN"
  | N_LAMBDA_END _ -> "N_LAMBDA_END"
  | N_UNKNOWN s -> Printf.sprintf "N_UNKNOWN %S" s
  | N_END -> "N_END"
  | N_SOURCE _ -> "N_SOURCE"
  | N_LEFT _ -> "N_LEFT"
  | N_RIGHT _ -> "N_RIGHT"
