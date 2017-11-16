(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let mic_of_tez { tezzies ; centiles } =
  match centiles with
  | None -> tezzies
  | Some centiles -> tezzies ^ "." ^ centiles

let mic_of_integer { integer } = integer

let int_of_integer { integer } = int_of_string integer
let integer_of_int int =
  let integer = string_of_int int in
  { integer }

let tez_of_mic s =
  let b = Buffer.create 10 in
  let parts = ref [] in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | ',' -> ()
    | '.' ->
       parts := (Buffer.contents b) :: !parts;
       Buffer.clear b
    | c -> Buffer.add_char b c
  done;
  let parts = Buffer.contents b :: !parts in
  match parts with
  | [ tezzies ]
  | [ "" ; tezzies ] -> { tezzies; centiles = None }
  | [ centiles; tezzies ] -> { tezzies; centiles = Some centiles }
  | _ -> invalid_arg "tez_of_mic" (* TODO exn *)

let integer_of_mic integer = { integer }

let remove_underscores s =
  let b = Buffer.create 10 in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
    | '_' -> ()
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let integer_of_liq s =
  let integer = remove_underscores s in
  { integer }

(* TODO: beware of overflow... *)
let tez_of_liq s =
  let s = remove_underscores s in
  try
    let pos = String.index s '.' in
    let len = String.length s in
    let tezzies = String.sub s 0 pos in
    let centiles = String.sub s (pos+1) (len - pos - 1) in
    let centiles_len = String.length centiles in
    let centiles = match centiles_len with
        0 -> None
      | 1 -> Some (centiles ^ "0")
      | 2 -> Some centiles
      | _ -> invalid_arg "bad centiles in tez_of_liq"
    in
    { tezzies; centiles }
  with Not_found ->
    { tezzies = s; centiles = None }

let liq_of_tez { tezzies ; centiles } =
  match centiles with
  | None -> tezzies
  | Some centiles -> tezzies ^ "." ^ centiles

let liq_of_integer { integer } = integer





let to_string bprinter x =
  let b = Buffer.create 10_000 in
  let indent = "  " in
  bprinter b indent x;
  Buffer.contents b

module Michelson = struct

(* For now, we always use the multi-line notation, and never output
  parenthesized expressions such as "(contract unit unit)" *)

  type format = {
      increase_indent : (string -> string);
      newline : char;
    }

  let multi_line = {
      increase_indent = (fun indent -> indent ^ "  ");
      newline = '\n';
    }
  let single_line = {
      increase_indent = (fun indent -> indent);
      newline = ' ';
    }

  let bprint_type_base bprint_type_rec b indent ty =
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
      | Tkey_hash  -> Printf.bprintf b "key_hash"
      | Tsignature  -> Printf.bprintf b "signature"
      | Ttuple tys -> bprint_type_pairs b indent tys
      | Trecord _ | Tsum _ -> assert false
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
    in
    bprint_type b indent ty

  let rec bprint_type b indent ty =
    bprint_type_base
      (fun b indent ty_name ty ->
        bprint_type b indent ty)
      b indent ty

  let rec bprint_const fmt b indent cst =
    match cst with
    | CString s -> Printf.bprintf b "%S" s
    | CKey s -> Printf.bprintf b "%S" s
    | CKey_hash s -> Printf.bprintf b "%S" s
    | CSignature s -> Printf.bprintf b "%S" s
    | CTez s -> Printf.bprintf b "%S" (mic_of_tez s)
    | CInt n -> Printf.bprintf b "%s" (mic_of_integer n)
    | CNat n -> Printf.bprintf b "%s" (mic_of_integer n)
    | CTimestamp s -> Printf.bprintf b "%S" s
    | CBool true -> Printf.bprintf b "True"
    | CBool false -> Printf.bprintf b "False"
    | CUnit -> Printf.bprintf b "Unit"
    | CNone -> Printf.bprintf b "None"
    | CSome cst ->
       let indent = fmt.increase_indent indent in
       Printf.bprintf b "(Some%c%s" fmt.newline indent;
       bprint_const fmt b indent cst;
       Printf.bprintf b "%c%s)" fmt.newline indent;
    | CLeft cst ->
       let indent = fmt.increase_indent indent in
       Printf.bprintf b "(Left%c%s" fmt.newline indent;
       bprint_const fmt b indent cst;
       Printf.bprintf b "%c%s)" fmt.newline indent;
    | CRight cst ->
       let indent = fmt.increase_indent indent in
       Printf.bprintf b "(Right%c%s" fmt.newline indent;
       bprint_const fmt b indent cst;
       Printf.bprintf b "%c%s)" fmt.newline indent;
    | CTuple tys -> bprint_const_pairs fmt b indent tys
    | CMap pairs ->
       let indent = fmt.increase_indent indent in
       Printf.bprintf b "(Map";
       List.iter (fun (cst1, cst2) ->
           Printf.bprintf b "%c%s(Item" fmt.newline indent;
           let indent = fmt.increase_indent indent in
           Printf.bprintf b "%c%s" fmt.newline indent;
           bprint_const fmt b indent cst1;
           Printf.bprintf b "%c%s" fmt.newline indent;
           bprint_const fmt b indent cst2;
           Printf.bprintf b "%c%s)" fmt.newline indent;
         ) pairs;
       Printf.bprintf b "%c%s)" fmt.newline indent;
    | CList csts ->
       let indent = fmt.increase_indent indent in
       Printf.bprintf b "(List";
       List.iter (fun cst ->
           Printf.bprintf b "%c%s" fmt.newline indent;
           bprint_const fmt b indent cst;
         ) csts;
       Printf.bprintf b "%c%s)" fmt.newline indent;
    | CSet csts ->
       let indent = fmt.increase_indent indent in
       Printf.bprintf b "(Set";
       List.iter (fun cst ->
           Printf.bprintf b "%c%s" fmt.newline indent;
           bprint_const fmt b indent cst;
         ) csts;
       Printf.bprintf b "%c%s)" fmt.newline indent;
    | CConstr _ | CRecord _ -> assert false

  and bprint_const_pairs fmt b indent tys =
    match tys with
    | [] -> assert false
    | [ty] -> bprint_const fmt b indent ty
    | ty :: tys ->
       let indent = fmt.increase_indent indent in
       Printf.bprintf b "(Pair%c%s" fmt.newline indent;
       bprint_const fmt b indent ty;
       Printf.bprintf b "%c%s" fmt.newline indent;
       bprint_const_pairs fmt b indent tys;
       Printf.bprintf b ")";
       ()

  let bprint_const_single b indent ty = bprint_const single_line b "" ty
  let bprint_const = bprint_const multi_line

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

  let bprint_pre_michelson bprint_arg b = function
    | SEQ args ->
      Printf.bprintf b "{ ";
      List.iter (fun a -> bprint_arg b a; Printf.bprintf b " ; ") args;
      Printf.bprintf b " }";
    | DIP (i, a) ->
      Printf.bprintf b "D%sP "
        (String.concat "" (LiquidMisc.list_init i (fun _ -> "I")));
      bprint_arg b a;
    | IF (a1, a2) ->
      Printf.bprintf b "IF ";
      bprint_arg b a1;
      bprint_arg b a2;
    | IF_NONE (a1, a2) ->
      Printf.bprintf b "IF_NONE ";
      bprint_arg b a1;
      bprint_arg b a2;
    | IF_CONS (a1, a2) ->
      Printf.bprintf b "IF_CONS ";
      bprint_arg b a1;
      bprint_arg b a2;
    | IF_LEFT (a1, a2) ->
      Printf.bprintf b "IF_LEFT ";
      bprint_arg b a1;
      bprint_arg b a2;
    | LOOP a ->
      Printf.bprintf b "LOOP ";
      bprint_arg b a;
    | LAMBDA (ty1, ty2, a) ->
      Printf.bprintf b "LAMBDA ";
      bprint_type b "" ty1;
      Printf.bprintf b " ";
      bprint_type b "" ty2;
      bprint_arg b a;
    | EXEC -> Printf.bprintf b "EXEC"
    | DUP i ->
      Printf.bprintf b "D%sP"
        (String.concat "" (LiquidMisc.list_init i (fun _ -> "U")));
    | DIP_DROP (i, r) ->
      Printf.bprintf b "DIP_DROP (%d, %d)" i r;
    | DROP -> Printf.bprintf b "DROP"
    | CAR -> Printf.bprintf b "CAR"
    | CDR -> Printf.bprintf b "CDR"
    | CDAR i ->
      Printf.bprintf b "C%sAR "
        (String.concat "" (LiquidMisc.list_init i (fun _ -> "D")));
    | CDDR i ->
      Printf.bprintf b "C%sDR "
        (String.concat "" (LiquidMisc.list_init i (fun _ -> "D")));
    | PUSH (ty, c) ->
      Printf.bprintf b "PUSH ";
      bprint_type b "" ty;
      bprint_const b "" c;
    | PAIR -> Printf.bprintf b "PAIR"
    | COMPARE -> Printf.bprintf b "COMPARE"
    | LE -> Printf.bprintf b "LE"
    | LT -> Printf.bprintf b "LT"
    | GE -> Printf.bprintf b "GE"
    | GT -> Printf.bprintf b "GT"
    | NEQ -> Printf.bprintf b "NEQ"
    | EQ -> Printf.bprintf b "EQ"
    | FAIL -> Printf.bprintf b "FAIL"
    | NOW -> Printf.bprintf b "NOW"
    | TRANSFER_TOKENS -> Printf.bprintf b "TRANSFER_TOKENS"
    | ADD -> Printf.bprintf b "ADD"
    | SUB -> Printf.bprintf b "SUB"
    | BALANCE -> Printf.bprintf b "BALANCE"
    | SWAP -> Printf.bprintf b "SWAP"
    | GET -> Printf.bprintf b "GET"
    | UPDATE -> Printf.bprintf b "UPDATE"
    | SOME -> Printf.bprintf b "SOME"
    | CONCAT -> Printf.bprintf b "CONCAT"
    | MEM -> Printf.bprintf b "MEM"
    | MAP -> Printf.bprintf b "MAP"
    | REDUCE -> Printf.bprintf b "REDUCE"
    | SELF -> Printf.bprintf b "SELF"
    | AMOUNT -> Printf.bprintf b "AMOUNT"
    | STEPS_TO_QUOTA -> Printf.bprintf b "STEPS_TO_QUOTA"
    | MANAGER -> Printf.bprintf b "MANAGER"
    | CREATE_ACCOUNT -> Printf.bprintf b "CREATE_ACCOUNT"
    | CREATE_CONTRACT -> Printf.bprintf b "CREATE_CONTRACT"
    | H -> Printf.bprintf b "H"
    | HASH_KEY -> Printf.bprintf b "HASH_KEY"
    | CHECK_SIGNATURE -> Printf.bprintf b "CHECK_SIGNATURE"
    | CONS -> Printf.bprintf b "CONS"
    | OR -> Printf.bprintf b "OR"
    | XOR -> Printf.bprintf b "XOR"
    | AND -> Printf.bprintf b "AND"
    | NOT -> Printf.bprintf b "NOT"
    | INT -> Printf.bprintf b "INT"
    | ABS -> Printf.bprintf b "ABS"
    | NEG -> Printf.bprintf b "NEG"
    | MUL -> Printf.bprintf b "MUL"
    | LEFT ty ->
      Printf.bprintf b "LEFT";
      bprint_type b "" ty;
    | RIGHT ty ->
      Printf.bprintf b "RIGHT";
      bprint_type b "" ty;
    | EDIV -> Printf.bprintf b "EDIV"
    | LSL -> Printf.bprintf b "LSL"
    | LSR -> Printf.bprintf b "LSR"
    | SOURCE (ty1, ty2) ->
      Printf.bprintf b "SOURCE";
      bprint_type b "" ty1;
      bprint_type b "" ty2;
    | SIZE -> Printf.bprintf b "SIZE"
    | DEFAULT_ACCOUNT -> Printf.bprintf b "DEFAULT_ACCOUNT"
    | MOD -> Printf.bprintf b "MOD"
    | DIV -> Printf.bprintf b "DIV"

  let rec bprint_noloc_michelson b ins=
    bprint_pre_michelson bprint_noloc_michelson b ins.i

  let rec bprint_loc_michelson b m =
    bprint_pre_michelson bprint_loc_michelson b m.ins


  let string_of_type = to_string bprint_type
  let string_of_code code = to_string bprint_code code
  let string_of_const = to_string bprint_const
  let line_of_const = to_string bprint_const_single
  let string_of_contract cmd = to_string (bprint_contract bprint_code) cmd
  let string_of_noloc_michelson = to_string (fun b _ -> bprint_noloc_michelson b)
  let string_of_loc_michelson = to_string (fun b _ -> bprint_loc_michelson b)

end



module Liquid = struct


  let bprint_type_base expand b indent ty =
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
      | Tkey_hash  -> Printf.bprintf b "key_hash"
      | Tsignature  -> Printf.bprintf b "signature"
      | Ttuple [] -> assert false
      | Ttuple (ty :: tys) ->
        Printf.bprintf b "(";
        bprint_type b "" ty;
        List.iter (fun ty ->
            Printf.bprintf b " * ";
            bprint_type b "" ty;
          ) tys;
        Printf.bprintf b ")";
      | Trecord (_, []) -> assert false
      | Trecord (_, (f, ty) :: rtys) when expand ->
        Printf.bprintf b "{ ";
        Printf.bprintf b "%s: " f;
        bprint_type b "" ty;
        List.iter (fun (f, ty) ->
            Printf.bprintf b "; %s: " f;
            bprint_type b "" ty;
          ) rtys;
        Printf.bprintf b " }";
      | Trecord (name, _) ->
        Printf.bprintf b "%s" name;
      | Tsum (_, []) -> assert false
      | Tsum (_, (c, ty) :: rtys) when expand ->
        Printf.bprintf b "%s of " c;
        bprint_type b "" ty;
        List.iter (fun (c, ty) ->
            Printf.bprintf b " | %s of " c;
            bprint_type b "" ty;
          ) rtys;
      | Tsum (name, _) ->
        Printf.bprintf b "%s" name;
      | Tcontract (ty1, ty2) ->
        Printf.bprintf b "(";
        bprint_type b "" ty1;
        Printf.bprintf b ", ";
        bprint_type b "" ty2;
        Printf.bprintf b ") contract";
      | Tor (ty1, ty2) ->
        Printf.bprintf b "(";
        bprint_type b "" ty1;
        Printf.bprintf b ", ";
        bprint_type b "" ty2;
        Printf.bprintf b ") variant";
      | Toption ty ->
        bprint_type b "" ty;
        Printf.bprintf b " option";
      | Tlist ty ->
        bprint_type b "" ty;
        Printf.bprintf b " list";
      | Tset ty ->
        bprint_type b "" ty;
        Printf.bprintf b " set";
      | Tmap (ty1, ty2) ->
        Printf.bprintf b "(";
        bprint_type b "" ty1;
        Printf.bprintf b ", ";
        bprint_type b "" ty2;
        Printf.bprintf b ") map";
      | Tlambda (ty1, ty2) ->
        bprint_type b "" ty1;
        Printf.bprintf b " -> ";
        bprint_type b "" ty2;
      | Tclosure ((ty_arg, ty_env), ty_r) ->
        bprint_type b "" ty_arg;
        Printf.bprintf b " {";
        bprint_type b "" ty_env;
        Printf.bprintf b "}-> ";
        bprint_type b "" ty_r;
    in
    bprint_type b indent ty

  let rec bprint_type ?(expand=false) b indent ty =
    bprint_type_base expand b indent ty


  let bprint_type2 b indent ty =
    let set = ref StringSet.empty in
    let todo = ref [None, ty] in
    let rec iter () =
      match !todo with
        [] -> ()
      | (ty_name, ty) :: rem ->
         todo := rem;
         let indent = match ty_name with
           | None -> indent
           | Some ty_name ->
              Printf.bprintf b "%s%s = " indent ty_name;
              indent ^ "  "
         in
         Michelson.bprint_type_base
           (fun b indent ty_name ty ->
             Printf.bprintf b "%s" ty_name;
             if not ( StringSet.mem ty_name !set ) then begin
                 set := StringSet.add ty_name !set;
                 todo := (Some ty_name, ty) :: !todo
               end
           )
           b indent ty;
         Printf.bprintf b "\n";
         iter ()
    in
    iter ()

  let rec bprint_const b indent cst =
    match cst with
    | CString s -> Printf.bprintf b "%S" s
    | CKey s -> Printf.bprintf b "%s" s
    | CKey_hash s -> Printf.bprintf b "%s" s
    | CSignature s -> Printf.bprintf b "%s" s
    | CTez s -> Printf.bprintf b "%S" (liq_of_tez s)
    | CInt n -> Printf.bprintf b "%s" (liq_of_integer n)
    | CNat n -> Printf.bprintf b "%s" (liq_of_integer n)
    | CTimestamp s -> Printf.bprintf b "%s" s
    | CBool v -> Printf.bprintf b "%b" v
    | CUnit -> Printf.bprintf b "()"
    | CNone -> Printf.bprintf b "None"
    | CSome cst ->
      Printf.bprintf b "(Some ";
      bprint_const b "" cst;
      Printf.bprintf b ")";
    | CLeft cst ->
      Printf.bprintf b "(Left ";
      bprint_const b "" cst;
      Printf.bprintf b ")";
    | CRight cst ->
      Printf.bprintf b "(Right ";
      bprint_const b "" cst;
      Printf.bprintf b ")";
    | CTuple [] -> assert false
    | CTuple (c :: cs) ->
      Printf.bprintf b "(";
      bprint_const b "" c;
      List.iter (fun c ->
          Printf.bprintf b " * ";
          bprint_const b "" c;
        ) cs;
      Printf.bprintf b ")";
    | CMap pairs ->
      Printf.bprintf b "(Map [";
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
        (fun _ (c1, c2) ->
           bprint_const b "" c1;
           Printf.bprintf b ", ";
           bprint_const b "" c2)
        (Format.formatter_of_buffer b) pairs;
      Printf.bprintf b "])";
    | CList csts ->
      Printf.bprintf b "[";
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
        (fun _ c -> bprint_const b "" c)
        (Format.formatter_of_buffer b) csts;
      Printf.bprintf b "]";
    | CSet csts ->
      Printf.bprintf b "(Set [";
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
        (fun _ c -> bprint_const b "" c)
        (Format.formatter_of_buffer b) csts;
      Printf.bprintf b "])"
    | CConstr (c, cst) ->
      Printf.bprintf b "(%s " c;
      bprint_const b "" cst;
      Printf.bprintf b ")";
    | CRecord labels ->
      Printf.bprintf b "{";
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
        (fun _ (f, cst) ->
           Printf.bprintf b "%s = " f;
           bprint_const b "" cst)
        (Format.formatter_of_buffer b) labels;
      Printf.bprintf b "}"


  let rec bprint_code_base bprint_code_rec ~debug b indent code =
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
       bprint_code_rec ~debug b indent2 exp;
       Printf.bprintf b "\n%sin" indent;
       bprint_code_rec ~debug b indent body
    | Const (ty, cst) ->
       Printf.bprintf b "\n%s" indent;
       bprint_const b indent cst;
    | SetVar (name, _loc, labels, e) ->
       let indent2 = indent ^ "  " in
       Printf.bprintf b "\n%s(%s <-" indent (String.concat "." (name :: labels));
       bprint_code_rec ~debug b indent2 e;
       Printf.bprintf b ")";
    | Var (name, _loc, labels) ->
       Printf.bprintf b "\n%s%s" indent (String.concat "." (name :: labels))
    | Apply (prim, _loc, args) ->
       Printf.bprintf b "\n%s(%s" indent
                      (LiquidTypes.string_of_primitive prim);
       let indent2 = indent ^ "  " in
       List.iter (fun exp ->
           bprint_code_rec ~debug b indent2 exp
         ) args;
       Printf.bprintf b ")"
    | If (cond, ifthen, ifelse) ->
       let indent2 = indent ^ "  " in
       Printf.bprintf b "\n%sif" indent;
       bprint_code_rec ~debug b indent2 cond;
       Printf.bprintf b "\n%sthen" indent;
       bprint_code_rec ~debug b indent2 ifthen;
       Printf.bprintf b "\n%selse" indent;
       bprint_code_rec ~debug b indent2 ifelse;
    | Seq (exp1, exp2) ->
       bprint_code_rec ~debug b indent exp1;
       Printf.bprintf b ";";
       bprint_code_rec ~debug b indent exp2
    | LetTransfer (storage, result, _loc, contract_exp, tez_exp,
                   storage_exp, arg_exp, body) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%slet %s, %s =" indent storage result;
       Printf.bprintf b "\n%sContract.call" indent2;
       bprint_code_rec ~debug b indent4 contract_exp;
       bprint_code_rec ~debug b indent4 tez_exp;
       bprint_code_rec ~debug b indent4 storage_exp;
       bprint_code_rec ~debug b indent4 arg_exp;
       Printf.bprintf b "\n%sin" indent;
       bprint_code_rec ~debug b indent body
    | MatchOption (arg, _loc, ifnone, var, ifsome) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%smatch " indent;
       bprint_code_rec ~debug b indent2 arg;
       Printf.bprintf b " with\n";
       Printf.bprintf b "\n%s| None ->\n" indent2;
       bprint_code_rec ~debug b indent4 ifnone;
       Printf.bprintf b "\n%s| Some %s ->\n" indent2 var;
       bprint_code_rec ~debug b indent4 ifsome;
       ()
    | MatchNat (arg, _loc, p, ifplus, m, ifminus) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%smatch%%nat " indent;
       bprint_code_rec ~debug b indent2 arg;
       Printf.bprintf b " with\n";
       Printf.bprintf b "\n%s| Plus %s ->\n" indent2 p;
       bprint_code_rec ~debug b indent4 ifplus;
       Printf.bprintf b "\n%s| Minus %s ->\n" indent2 m;
       bprint_code_rec ~debug b indent4 ifminus;
       ()
    | MatchList (arg, _loc, head_name, tail_name, ifcons, ifnil) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%smatch " indent;
       bprint_code_rec ~debug b indent2 arg;
       Printf.bprintf b " with\n";
       Printf.bprintf b "\n%s| [] ->\n" indent2;
       bprint_code_rec ~debug b indent4 ifnil;
       Printf.bprintf b "\n%s| %s :: %s ->\n" indent2 head_name tail_name;
       bprint_code_rec ~debug b indent4 ifcons;
       ()
    | Loop (name, _loc, body, arg) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%sLoop.loop (fun %s -> " indent name;
       bprint_code_rec ~debug b indent4 body;
       Printf.bprintf b ")\n%s" indent2;
       bprint_code_rec ~debug b indent2 arg;
       ()
    | Closure (arg_name, arg_type, _loc, _, body, res_type)
    (* FIXME change this *)
    | Lambda (arg_name, arg_type, _loc, body, res_type) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%s(fun ( %s : " indent arg_name;
       bprint_type b indent2 arg_type;
       Printf.bprintf b ") ->\n%s" indent2;
       bprint_code_rec ~debug b indent4 body;
       Printf.bprintf b ")"
    | Record (_loc, lab_x_exp_list) ->
       let indent2 = indent ^ "  " in
       let indent4 = indent2 ^ "  " in
       Printf.bprintf b "\n%s{" indent;
       List.iter (fun (label, exp) ->
           Printf.bprintf b "\n%s%s = " indent2 label;
           bprint_code_rec ~debug b indent4 exp;
           Printf.bprintf b ";";
         ) lab_x_exp_list;
       Printf.bprintf b "}"
    | Constructor (_loc, Constr constr, arg) ->
       Printf.bprintf b "\n%s%s (" indent constr;
       bprint_code_rec ~debug b (indent ^ "  ") arg;
       Printf.bprintf b ")"
    | Constructor (_loc, Left right_ty, arg) ->
       Printf.bprintf b "\n%s(Left " indent;
       bprint_code_rec ~debug b (indent ^ "  ") arg;
       Printf.bprintf b " : (_, ";
       bprint_type b (indent ^ "  ") right_ty;
       Printf.bprintf b ") variant)"
    | Constructor (_loc, Right right_ty, arg) ->
       Printf.bprintf b "\n%s(Right " indent;
       bprint_code_rec ~debug b (indent ^ "  ") arg;
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
       bprint_code_rec ~debug b indent2 arg;
       Printf.bprintf b " with\n";
       List.iter (function
           | CConstr (constr, vars), e ->
             Printf.bprintf b "\n%s| %s (%s) ->\n" indent2 constr
               (String.concat ", " vars);
             bprint_code_rec ~debug b indent4 e;
           | CAny, e ->
             Printf.bprintf b "\n%s| _ ->\n" indent2;
             bprint_code_rec ~debug b indent4 e;
         ) cases;
       ()

  let rec bprint_code_types ~debug b indent code =
    bprint_code_base
      (fun ~debug b indent code ->
         bprint_code_types ~debug b indent code;
         Printf.bprintf b "\n%s(* : " indent;
         bprint_type b (indent^"  ") code.ty;
         Printf.bprintf b " *)";
      )
      ~debug b indent code

  let rec bprint_code ~debug b indent code =
    bprint_code_base bprint_code ~debug b indent code

  let bprint_contract bprint_code ~debug b indent contract =
    let indent2 = indent ^ "    " in
    Printf.bprintf b "let%%entry main\n";
    (* Printf.bprintf b "    (amount: tez)\n"; *)
    Printf.bprintf b "    (parameter/2: ";
    bprint_type b indent2 contract.parameter;
    Printf.bprintf b ")\n";
    Printf.bprintf b "    (storage/1: ";
    bprint_type b indent2 contract.storage;
    Printf.bprintf b ")\n";
    Printf.bprintf b "    : ";
    bprint_type b indent2 (Ttuple [contract.return; contract.storage]);
    Printf.bprintf b "= \n";

    bprint_code ~debug b indent contract.code

  let string_of_type = to_string bprint_type
  let string_of_type_expl = to_string (fun b -> bprint_type ~expand:true b)
  let string_of_const = to_string bprint_const
  let string_of_code ?(debug=false) code =
    to_string (bprint_code ~debug) code
  let string_of_code_types ?(debug=false) code =
    to_string (bprint_code_types ~debug) code
  let string_of_contract ?(debug=false) cmd =
    to_string (bprint_contract bprint_code ~debug) cmd
  let string_of_contract_types ?(debug=false) cmd =
    to_string (bprint_contract bprint_code_types ~debug) cmd

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
  | N_IF_PLUS _ -> "N_IF_PLUS"
  | N_IF_MINUS _ -> "N_IF_MINUS"
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
  | N_ABS -> "N_ABS"
