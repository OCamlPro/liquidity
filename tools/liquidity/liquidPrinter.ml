(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                             Steven De Oliveira                           *)
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

  let to_string fmt bprinter x =
    let b = Buffer.create 10_000 in
    let indent = fmt.increase_indent "" in
    bprinter fmt b indent x;
    Buffer.contents b

  let bprint_annots ?(parameter=false) b annots =
    if parameter || not !LiquidOptions.no_annot then
      match annots with
      | [] -> ()
      | _ -> Printf.bprintf b "%s" (String.concat " " ("" :: annots))

  let bprint_wrap_annots ?parameter b bprint_type annots =
    match annots with
    | [] -> bprint_type ()
    | _ ->
      Printf.bprintf b "(";
      bprint_type ();
      bprint_annots ?parameter b annots;
      Printf.bprintf b ")"

  let is_word_type = function
    | Tfail | Tunit | Tbool | Tint   | Tnat | Ttez | Tstring | Tbytes
    | Ttimestamp | Tkey | Tkey_hash | Tsignature | Toperation | Taddress | Tchainid ->
      true
    | Ttuple _ | Trecord _ | Tsum _ | Tcontract_handle _ | Tcontract_view _
    | Tor _ | Toption _ | Tlist _
    | Tset _ | Tmap _ | Tbigmap _ | Tlambda _ | Tclosure _ ->
      false
    | Tvar _ -> true
    | Tpartial _ -> false

  let bprint_type_base ?parameter fmt b indent ty annots =
    let rec bprint_type_rec fmt b indent ty annots =
      match ty with
      | Tfail -> Printf.bprintf b "unit" (* use unit for failure in michelson *)
      | Tunit -> Printf.bprintf b "unit"
      | Tbool -> Printf.bprintf b "bool"
      | Tint -> Printf.bprintf b "int"
      | Tnat -> Printf.bprintf b "nat"
      | Ttez -> Printf.bprintf b "%s" (LiquidOptions.mu_amount_type ())
      | Tstring -> Printf.bprintf b "string"
      | Tbytes -> Printf.bprintf b "bytes"
      | Ttimestamp  -> Printf.bprintf b "timestamp"
      | Tkey  -> Printf.bprintf b "key"
      | Tkey_hash  -> Printf.bprintf b "key_hash"
      | Tsignature  -> Printf.bprintf b "signature"
      | Toperation  -> Printf.bprintf b "operation"
      | Taddress  -> Printf.bprintf b "address"
      | Tchainid  -> Printf.bprintf b "chain_id"
      | Ttuple tys ->
        bprint_type_pairs fmt b indent tys annots
      | Trecord (name, labels) ->
        bprint_type_record name fmt b indent labels annots
      | Tsum (name, constrs) ->
        bprint_type_sum name fmt b indent constrs annots
      | Tcontract_handle (_entry, ty) ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(contract";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty [];
        Printf.bprintf b ")";
      | Tcontract_view _ -> assert false
      | Tor (ty1, ty2) ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(or";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty1 [];
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty2 [];
        Printf.bprintf b ")";
      | Toption ty ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(option";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty [];
        Printf.bprintf b ")";
      | Tlist ty ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(list";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty [];
        Printf.bprintf b ")";
      | Tset ty ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(set";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty [];
        Printf.bprintf b ")";
      | Tmap (ty1, ty2) ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(map";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty1 [];
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty2 [];
        Printf.bprintf b ")";
      | Tbigmap (ty1, ty2) ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(big_map";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty1 [];
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty2 [];
        Printf.bprintf b ")";
      | Tlambda (ty1, ty2, _) ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(lambda";
        bprint_annots b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty1 [];
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty2 [];
        Printf.bprintf b ")";
      | Tclosure ((ty_arg, ty_env), ty_r, u) ->
        bprint_type fmt b indent
          (Ttuple [Tlambda (Ttuple [ty_arg; ty_env], ty_r, u);
                   ty_env ]) annots;
      | Tvar tvr ->
        Printf.bprintf b "'%s (" (Ref.get tvr).id;
        begin match (Ref.get tvr).tyo with
          | Some ty -> bprint_type fmt b indent (LiquidTypes.expand ty) []
          | None -> Printf.bprintf b "-" end;
        Printf.bprintf b ")"
      | Tpartial (Peqn _) -> Printf.bprintf b "peqn"
      | Tpartial (Ptup pl) ->
        Printf.bprintf b "(ptup:";
        List.iter (fun (n,t) ->
            let id = match t with Tvar tvr -> (Ref.get tvr).id | _ -> "-" in
            Printf.bprintf b " %d:%s" n id) pl;
        Printf.bprintf b ")";
      | Tpartial (Pmap _) -> Printf.bprintf b "pmap"
      | Tpartial (Pcont _) -> Printf.bprintf b "pcont"
      | Tpartial (Ppar) -> Printf.bprintf b "ppar"

    and bprint_type_pairs fmt b indent tys annots =
      match tys with
      | [] -> assert false
      | [ty] -> bprint_type fmt b indent ty annots
      | ty :: tys ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(pair";
        bprint_annots ?parameter b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty [];
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type_pairs fmt b indent tys [];
        Printf.bprintf b ")";
        ()

    and bprint_type_composed ty_c name fmt b indent labels annots =
      let annots = match name with
        | None | Some "" -> annots
        | Some name ->
          let name =
            if name.[0] = '`' then String.sub name 1 (String.length name - 1)
            else name in
          (":" ^ name) :: annots in
      match labels with
      | [] -> assert false
      | [label, ty] ->
        let annots = match ty with
          | Tbigmap _ -> (":" ^ label) :: annots
          | _ -> ("%" ^ label) :: annots in
        bprint_type fmt b indent ty annots;
      | [label_bigmap, (Tbigmap _ as ty_b); label_r, ty_r] ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(%s" ty_c;
        bprint_annots ?parameter b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty_b [];
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type fmt b indent ty_r [];
        Printf.bprintf b ")"
      | (label, ty) :: labels ->
        let indent = fmt.increase_indent indent in
        Printf.bprintf b "(%s" ty_c;
        bprint_annots ?parameter b annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        let annots = match ty with
          | Tbigmap _ -> [":" ^ label]
          | _ -> ["%" ^ label] in
        bprint_type fmt b indent ty annots;
        Printf.bprintf b "%c%s" fmt.newline indent;
        bprint_type_composed ty_c None fmt b indent labels [];
        Printf.bprintf b ")"

    and bprint_type_record name fmt b indent labels annots =
      bprint_type_composed "pair" (Some name) fmt b indent labels annots

    and bprint_type_sum name fmt b indent constrs annots =
      bprint_type_composed "or" name fmt b indent constrs annots

    and bprint_type fmt b indent ty annots =
      if is_word_type ty then
        bprint_wrap_annots ?parameter b
          (fun () -> bprint_type_rec fmt b indent ty []) annots
      else
        bprint_type_rec fmt b indent ty annots

    in
    bprint_type fmt b indent ty annots

  let rec bprint_type ?parameter fmt b indent ty =
    bprint_type_base ?parameter fmt b indent ty []

  let annot a =
    match a with
    | Some s -> " @" ^ s
    | None -> ""

  let annots_to_string annots =
    match annots with
    | [] -> ""
    | annots -> " " ^ String.concat " " annots

  let rec bprint_const (bprint_mic : format -> Buffer.t -> string -> 'a -> unit)
      fmt b ?(inseq=false) indent (cst : 'a const) =
    match cst with
    | CString s -> Printf.bprintf b "%S" s
    | CBytes s -> Printf.bprintf b "%s" s
    | CKey s -> Printf.bprintf b "%S" s
    | CKey_hash s -> Printf.bprintf b "%S" s
    | CSignature s -> Printf.bprintf b "%S" s
    | CContract (s, None) -> Printf.bprintf b "%S" s
    | CContract (s, Some entry) -> Printf.bprintf b "%S" (String.concat "%" [s; entry])
    | CTez s -> Printf.bprintf b "%s" (Z.to_string (LiquidNumber.mic_mutez_of_tez s))
    | CInt n -> Printf.bprintf b "%s" (Z.to_string (LiquidNumber.mic_of_integer n))
    | CNat n -> Printf.bprintf b "%s" (Z.to_string (LiquidNumber.mic_of_integer n))
    | CTimestamp s -> Printf.bprintf b "%S" s
    | CBool true -> Printf.bprintf b "True"
    | CBool false -> Printf.bprintf b "False"
    | CUnit -> Printf.bprintf b "Unit"
    | CNone -> Printf.bprintf b "None"
    | CSome cst ->
      let indent = fmt.increase_indent indent in
      if not inseq then Printf.bprintf b "(";
      Printf.bprintf b "Some%c%s" fmt.newline indent;
      bprint_const bprint_mic fmt b indent cst;
      if not inseq then Printf.bprintf b ")";
    | CLeft cst ->
      let indent = fmt.increase_indent indent in
      if not inseq then Printf.bprintf b "(";
      Printf.bprintf b "Left%c%s" fmt.newline indent;
      bprint_const bprint_mic fmt b indent cst;
      if not inseq then Printf.bprintf b ")";
    | CRight cst ->
      let indent = fmt.increase_indent indent in
      if not inseq then Printf.bprintf b "(";
      Printf.bprintf b "Right%c%s" fmt.newline indent;
      bprint_const bprint_mic fmt b indent cst;
      if not inseq then Printf.bprintf b ")";
    | CTuple tys -> bprint_const_pairs bprint_mic fmt b ~inseq indent tys
    | CMap pairs | CBigMap BMList pairs ->
      let indent = fmt.increase_indent indent in
      Printf.bprintf b "{";
      let _ = List.fold_left (fun first (cst1, cst2) ->
          if not first then Printf.bprintf b " ;";
          Printf.bprintf b "%c%sElt" fmt.newline indent;
          Printf.bprintf b "%c%s" fmt.newline indent;
          let indent = fmt.increase_indent indent in
          Printf.bprintf b "%c%s" fmt.newline indent;
          bprint_const bprint_mic fmt b indent cst1;
          Printf.bprintf b "%c%s" fmt.newline indent;
          bprint_const bprint_mic fmt b indent cst2;
          false
        ) true pairs
      in
      Printf.bprintf b "}";
    | CBigMap BMId id ->
      Printf.bprintf b "%s" (Z.to_string (LiquidNumber.mic_of_integer id))
    | CList csts | CSet csts ->
      let indent = fmt.increase_indent indent in
      Printf.bprintf b "{";
      let _ = List.fold_left (fun first cst ->
          if not first then Printf.bprintf b " ;";
          Printf.bprintf b "%c%s" fmt.newline indent;
          bprint_const bprint_mic fmt b ~inseq:true indent cst;
          false
        ) true csts
      in
      Printf.bprintf b "}";
    | CRecord fields ->
      List.map snd fields
      |> bprint_const_pairs bprint_mic fmt b indent
    | CLambda lam ->
      bprint_mic fmt b indent lam.body
    | CConstr _ -> assert false

  and bprint_const_pairs bprint_mic fmt b ?(inseq=false) indent tys =
    match tys with
    | [] -> assert false
    | [ty] -> bprint_const bprint_mic fmt b ~inseq indent ty
    | ty :: tys ->
      let indent = fmt.increase_indent indent in
      if not inseq then Printf.bprintf b "(";
      Printf.bprintf b "Pair%c%s" fmt.newline indent;
      bprint_const bprint_mic fmt b indent ty;
      Printf.bprintf b "%c%s" fmt.newline indent;
      bprint_const_pairs bprint_mic fmt b ~inseq:false indent tys;
      if not inseq then Printf.bprintf b ")";
      ()

  let rec bprint_code fmt b indent code =
    match code with
    | M_INS (ins, annots) ->
      Printf.bprintf b "%s%s ;" ins (annots_to_string annots)
    | M_INS_N (ins, n, annots) ->
      Printf.bprintf b "%s%s %d ;" ins (annots_to_string annots) n
    | M_INS_CST (ins, ty, cst, annots) ->
      let indent = fmt.increase_indent indent in
      Printf.bprintf b "%s%s%c%s"
        ins (annots_to_string annots) fmt.newline indent;
      bprint_type fmt b indent ty;
      Printf.bprintf b "%c%s" fmt.newline indent;
      bprint_const bprint_code fmt b indent cst;
      Printf.bprintf b " ;";
    | M_INS_EXP ("SEQ", [], [], annots) ->
      Printf.bprintf b "{%s}" (annots_to_string annots)
    | M_INS_EXP ("SEQ", [], exps, annots) ->
      Printf.bprintf b "{%s" (annots_to_string annots);
      let indent_in = fmt.increase_indent indent in
      List.iter (fun exp ->
          Printf.bprintf b "%c%s" fmt.newline indent_in;
          bprint_code fmt b indent_in exp) exps;
      Printf.bprintf b "%c%s}" fmt.newline indent
    | M_INS_EXP (ins,tys, exps, annots) ->
      let parameter = ins = "parameter" in
      let indent = fmt.increase_indent indent in
      Printf.bprintf b "%s%s" ins (annots_to_string annots);
      List.iter (fun ty ->
          Printf.bprintf b "%c%s" fmt.newline indent;
          bprint_type ~parameter fmt b indent ty) tys;
      List.iter (fun exp ->
          Printf.bprintf b "%c%s" fmt.newline indent;
          bprint_code fmt b indent exp) exps;
      Printf.bprintf b "%c%s;" fmt.newline indent;
    | M_INS_EXP_N (ins, n, exps, annots) ->
      let indent = fmt.increase_indent indent in
      Printf.bprintf b "%s%s" ins (annots_to_string annots);
      Printf.bprintf b "%c%s%d" fmt.newline indent n;
      List.iter (fun exp ->
          Printf.bprintf b "%c%s" fmt.newline indent;
          bprint_code fmt b indent exp) exps;
      Printf.bprintf b "%c%s;" fmt.newline indent;
      ()

(*
  let json_annot = function
    | Some s -> Printf.sprintf {|;"annot":%S|} ("@"^s)
    | None -> ""

  let rec bprint_code_json fmt b code =
    match code with
    | M_INS_ANNOT s -> () (* ignore *)
    | M_INS (ins, name) ->
      Printf.bprintf b {|{"prim":%S;"args":[]%s}|} ins (json_annot name)
    | M_INS_CST (ins,ty,cst,name) ->
      Printf.bprintf b {|{"prim":%S;"args":[|} ins;
      bprint_type_json fmt b ty;
      Printf.bprintf b ",";
      bprint_const_json fmt b cst;
      Printf.bprintf b "]%s}" (json_annot name);
    | M_INS_EXP ("SEQ", [], [], name) ->
      () (* ignore *)
    | M_INS_EXP ("SEQ", [], e :: exps, name) ->
      Printf.bprintf b "[";
      bprint_code_json fmt b e;
      List.iter (fun e ->
          Printf.bprintf b ",";
          bprint_code_json fmt b e;
        ) exps;
      Printf.bprintf b "]";
    | M_INS_EXP (ins,tys, exps, name) ->
      Printf.bprintf b {|{"prim":%S;"args":[|} ins;
      List.iter (fun ty ->
          bprint_type_json fmt b ty;
          Printf.bprintf b ",";
        ) tys;
      (match exps with
       | [] -> assert false;
       | [e] -> bprint_code_json fmt b e
       | e :: exps ->
         bprint_code_json fmt b e;
         List.iter (fun e ->
             Printf.bprintf b ",";
             bprint_code_json fmt b e;
           ) exps;
      );
      Printf.bprintf b "]%s}" (json_annot name);
      ()
*)

  let bprint_contract bprint_code fmt b indent contract =
    List.iter (fun exp ->
        bprint_code fmt b indent exp ;
        Printf.bprintf b "%c" fmt.newline;
      ) contract

  let bprint_pre_name b name =
    if not !LiquidOptions.no_annot then
      match name with
      | Some name -> Printf.bprintf b " @%s " name
      | None -> ()

  let bprint_pre_field b f =
    if not !LiquidOptions.no_annot then
      match f with
      | Some field -> Printf.bprintf b " %%%s " field
      | None -> ()

  let bprint_pre_entry b e = bprint_pre_field b e

  let rec bprint_pre_michelson bprint_arg fmt b name
      (m  : 'a pre_michelson) = match m with
    (* = function *)
    | RENAME name ->
      Printf.bprintf b "RENAME";
      bprint_pre_name b name;
    | SEQ args ->
      Printf.bprintf b "{ ";
      List.iter (fun a -> bprint_arg fmt b a; Printf.bprintf b " ; ") args;
      Printf.bprintf b " }";
    | DIP (1, a) ->
      Printf.bprintf b "DIP ";
      bprint_pre_name b name;
      bprint_arg fmt b a;
    | DIP (i, a) ->
      Printf.bprintf b "DIP %n " i;
      bprint_pre_name b name;
      bprint_arg fmt b a;
    | IF (a1, a2) ->
      Printf.bprintf b "IF ";
      bprint_pre_name b name;
      bprint_arg fmt b a1;
      bprint_arg fmt b a2;
    | IF_NONE (a1, a2) ->
      Printf.bprintf b "IF_NONE ";
      bprint_pre_name b name;
      bprint_arg fmt b a1;
      bprint_arg fmt b a2;
    | IF_CONS (a1, a2) ->
      Printf.bprintf b "IF_CONS ";
      bprint_pre_name b name;
      bprint_arg fmt b a1;
      bprint_arg fmt b a2;
    | IF_LEFT (a1, a2) ->
      Printf.bprintf b "IF_LEFT ";
      bprint_pre_name b name;
      bprint_arg fmt b a1;
      bprint_arg fmt b a2;
    | LOOP a ->
      Printf.bprintf b "LOOP ";
      bprint_pre_name b name;
      bprint_arg fmt b a;
    | LOOP_LEFT a ->
      Printf.bprintf b "LOOP_LEFT ";
      bprint_pre_name b name;
      bprint_arg fmt b a;
    | ITER a ->
      Printf.bprintf b "ITER ";
      bprint_pre_name b name;
      bprint_arg fmt b a;
    | MAP a ->
      Printf.bprintf b "MAP ";
      bprint_pre_name b name;
      bprint_arg fmt b a;
    | LAMBDA (ty1, ty2, a) ->
      Printf.bprintf b "LAMBDA ";
      bprint_pre_name b name;
      bprint_type fmt b "" ty1;
      Printf.bprintf b " ";
      bprint_type fmt b "" ty2;
      bprint_arg fmt b a;
    | EXEC ->
      Printf.bprintf b "EXEC";
      bprint_pre_name b name;
    | DUP i ->
      Printf.bprintf b "D%sP"
        (String.concat "" (LiquidMisc.list_init i (fun _ -> "U")));
      bprint_pre_name b name;
    | DIP_DROP (i, r) ->
      Printf.bprintf b "DIP %d { DROP %d }" i r;
      bprint_pre_name b name;
    | DROP n ->
      Printf.bprintf b "DROP %d" n;
      bprint_pre_name b name;
    | DIG n ->
      Printf.bprintf b "DIG %d" n;
      bprint_pre_name b name;
    | DUG n ->
      Printf.bprintf b "DUG %d" n;
      bprint_pre_name b name;
    | CAR field ->
      Printf.bprintf b "CAR";
      bprint_pre_name b name;
      bprint_pre_field b field;
    | CDR field ->
      Printf.bprintf b "CDR";
      bprint_pre_name b name;
      bprint_pre_field b field;
    | CDAR (i, field) ->
      Printf.bprintf b "C%sAR "
        (String.concat "" (LiquidMisc.list_init i (fun _ -> "D")));
      bprint_pre_name b name;
      bprint_pre_field b field;
    | CDDR (i, field) ->
      Printf.bprintf b "C%sDR "
        (String.concat "" (LiquidMisc.list_init i (fun _ -> "D")));
      bprint_pre_name b name;
      bprint_pre_field b field;
    | PUSH (ty, c) ->
      Printf.bprintf b "PUSH ";
      bprint_pre_name b name;
      bprint_type fmt b "" ty;
      Printf.bprintf b " ";
      bprint_const (fun fmt b ident a -> bprint_arg fmt b a) fmt b "" c;
    | EMPTY_BIG_MAP (k, v) ->
      Printf.bprintf b "EMPTY_BIG_MAP ";
      bprint_pre_name b name;
      bprint_type fmt b "" k;
      bprint_type fmt b "" v;
    | PAIR ->
      Printf.bprintf b "PAIR";
      bprint_pre_name b name;
    | RECORD (f1, f2) ->
      Printf.bprintf b "PAIR";
      bprint_pre_name b name;
      bprint_pre_field b (Some f1);
      bprint_pre_field b f2;
    | COMPARE ->
      Printf.bprintf b "COMPARE";
      bprint_pre_name b name;
    | LE ->
      Printf.bprintf b "LE";
      bprint_pre_name b name;
    | LT ->
      Printf.bprintf b "LT";
      bprint_pre_name b name;
    | GE ->
      Printf.bprintf b "GE";
      bprint_pre_name b name;
    | GT ->
      Printf.bprintf b "GT";
      bprint_pre_name b name;
    | NEQ ->
      Printf.bprintf b "NEQ";
      bprint_pre_name b name;
    | EQ ->
      Printf.bprintf b "EQ";
      bprint_pre_name b name;
    | FAILWITH ->
      Printf.bprintf b "FAILWITH";
    | NOW ->
      Printf.bprintf b "NOW";
      bprint_pre_name b name;
    | TRANSFER_TOKENS ->
      Printf.bprintf b "TRANSFER_TOKENS";
      bprint_pre_name b name;
    | ADD ->
      Printf.bprintf b "ADD";
      bprint_pre_name b name;
    | SUB ->
      Printf.bprintf b "SUB";
      bprint_pre_name b name;
    | BALANCE ->
      Printf.bprintf b "BALANCE";
      bprint_pre_name b name;
    | SWAP ->
      Printf.bprintf b "SWAP";
      bprint_pre_name b name;
    | GET ->
      Printf.bprintf b "GET";
      bprint_pre_name b name;
    | UPDATE ->
      Printf.bprintf b "UPDATE";
      bprint_pre_name b name;
    | SOME ->
      Printf.bprintf b "SOME";
      bprint_pre_name b name;
    | CONCAT ->
      Printf.bprintf b "CONCAT";
      bprint_pre_name b name;
    | SLICE ->
      Printf.bprintf b "SLICE";
      bprint_pre_name b name;
    | MEM ->
      Printf.bprintf b "MEM";
      bprint_pre_name b name;
    | SELF e ->
      Printf.bprintf b "SELF";
      bprint_pre_name b name;
      bprint_pre_entry b e;
    | AMOUNT ->
      Printf.bprintf b "AMOUNT";
      bprint_pre_name b name;
    | STEPS_TO_QUOTA ->
      Printf.bprintf b "STEPS_TO_QUOTA";
      bprint_pre_name b name;
    | ADDRESS ->
      Printf.bprintf b "ADDRESS";
      bprint_pre_name b name;
    | CHAIN_ID ->
      Printf.bprintf b "CHAIN_ID";
      bprint_pre_name b name;
    | CREATE_CONTRACT contract ->
      Printf.bprintf b "CREATE_CONTRACT { parameter ";
      bprint_type fmt b "" contract.mic_parameter;
      Printf.bprintf b " ; storage ";
      bprint_type fmt b "" contract.mic_storage;
      Printf.bprintf b " ; code ";
      bprint_arg fmt b contract.mic_code;
      Printf.bprintf b " }";
      bprint_pre_name b name;
    | PACK ->
      Printf.bprintf b "PACK";
      bprint_pre_name b name;
    | UNPACK ty ->
      Printf.bprintf b "UNPACK";
      bprint_pre_name b name;
      bprint_type fmt b "" ty;
    | BLAKE2B ->
      Printf.bprintf b "BLAKE2B";
      bprint_pre_name b name;
    | SHA256 ->
      Printf.bprintf b "SHA256";
      bprint_pre_name b name;
    | SHA512 ->
      Printf.bprintf b "SHA512";
      bprint_pre_name b name;
    | HASH_KEY ->
      Printf.bprintf b "HASH_KEY";
      bprint_pre_name b name;
    | CHECK_SIGNATURE ->
      Printf.bprintf b "CHECK_SIGNATURE";
      bprint_pre_name b name;
    | CONS ->
      Printf.bprintf b "CONS";
      bprint_pre_name b name;
    | OR ->
      Printf.bprintf b "OR";
      bprint_pre_name b name;
    | XOR ->
      Printf.bprintf b "XOR";
      bprint_pre_name b name;
    | AND ->
      Printf.bprintf b "AND";
      bprint_pre_name b name;
    | NOT ->
      Printf.bprintf b "NOT";
      bprint_pre_name b name;
    | INT ->
      Printf.bprintf b "INT";
      bprint_pre_name b name;
    | ISNAT ->
      Printf.bprintf b "ISNAT";
      bprint_pre_name b name;
    | ABS ->
      Printf.bprintf b "ABS";
      bprint_pre_name b name;
    | NEG ->
      Printf.bprintf b "NEG";
      bprint_pre_name b name;
    | MUL ->
      Printf.bprintf b "MUL";
      bprint_pre_name b name;
    | LEFT (ty, constr) ->
      Printf.bprintf b "LEFT";
      bprint_pre_name b name;
      bprint_pre_field b constr;
      bprint_type fmt b "" ty;
    | RIGHT (ty, constr) ->
      Printf.bprintf b "RIGHT";
      bprint_pre_name b name;
      bprint_pre_field b constr;
      bprint_type fmt b "" ty;
    | CONTRACT (e, ty) ->
      Printf.bprintf b "CONTRACT";
      bprint_pre_name b name;
      bprint_pre_entry b e;
      bprint_type fmt b "" ty;
    | EDIV ->
      Printf.bprintf b "EDIV";
      bprint_pre_name b name;
    | LSL ->
      Printf.bprintf b "LSL";
      bprint_pre_name b name;
    | LSR ->
      Printf.bprintf b "LSR";
      bprint_pre_name b name;
    | SOURCE ->
      Printf.bprintf b "SOURCE";
      bprint_pre_name b name;
    | SENDER ->
      Printf.bprintf b "SENDER";
      bprint_pre_name b name;
    | SIZE ->
      Printf.bprintf b "SIZE";
      bprint_pre_name b name;
    | IMPLICIT_ACCOUNT ->
      Printf.bprintf b "IMPLICIT_ACCOUNT";
      bprint_pre_name b name;
    | SET_DELEGATE ->
      Printf.bprintf b "SET_DELEGATE";
      bprint_pre_name b name;
    | MOD ->
      Printf.bprintf b "MOD";
      bprint_pre_name b name;
    | DIV ->
      Printf.bprintf b "DIV";
      bprint_pre_name b name
    | EXTENSION (minst, tys) ->
      Printf.bprintf b "%S" minst;
      bprint_pre_name b name;
      List.iter (fun ty -> bprint_type fmt b " " ty) tys
    | BLOCK_LEVEL ->
      Printf.bprintf b "BLOCK_LEVEL";
      bprint_pre_name b name;
    | COLLECT_CALL ->
      Printf.bprintf b "COLLECT_CALL";
      bprint_pre_name b name;
    | GET_BALANCE ->
      Printf.bprintf b "GET_BALANCE";
      bprint_pre_name b name;
    | IS_IMPLICIT ->
      Printf.bprintf b "IS_IMPLICIT";
      bprint_pre_name b name

  let rec bprint_loc_michelson fmt b m =
    bprint_pre_michelson bprint_loc_michelson fmt b m.loc_name m.ins

  (* remove optional argument inseq *)
  let bprint_const fmt b indent cst = bprint_const fmt b indent cst

  let string_of_type = to_string multi_line bprint_type
  let line_of_type = to_string single_line bprint_type
  let string_of_code code = to_string multi_line bprint_code code
  let line_of_code code = to_string single_line bprint_code code
  let string_of_const c = to_string multi_line (bprint_const bprint_code) c
  let line_of_const c = to_string single_line (bprint_const bprint_code) c
  let string_of_contract cmd =
    to_string multi_line (bprint_contract bprint_code) cmd
  let line_of_contract cmd =
    to_string single_line (bprint_contract bprint_code) cmd
  let string_of_loc_michelson =
    to_string multi_line (fun fmt b _ -> bprint_loc_michelson fmt b)
  let line_of_loc_michelson =
    to_string single_line (fun fmt b _ -> bprint_loc_michelson fmt b)
  let string_of_loc_michelson_const c =
    to_string multi_line (bprint_const (fun fmt b _ -> bprint_loc_michelson fmt b)) c

end


module Syntax = struct
  open Format

  let string_of_core_type ty =
    ignore (flush_str_formatter ()) ;
    let f = str_formatter in
    let ty = Reason_toolchain.From_current.copy_core_type ty in
    let print =
      if !LiquidOptions.ocaml_syntax
      then Reason_toolchain.ML.print_core_type
      else Reason_toolchain.RE.print_core_type in
    print f ty;
    flush_str_formatter ()

  let string_of_expression x =
    ignore (flush_str_formatter ()) ;
    let f = str_formatter in
    let x = Reason_toolchain.From_current.copy_expression x in
    let print =
      if !LiquidOptions.ocaml_syntax then
      Reason_toolchain.ML.print_expression
    else
      Reason_toolchain.RE.print_expression in
    print f x;
    flush_str_formatter ()

  let string_of_structure str coms =
    ignore (flush_str_formatter ());
    let ppf = str_formatter in
    let reason_comments = List.map (fun (text, location) ->
        Reason_comment.make ~location Regular text
      ) coms in
    let str = Reason_toolchain.From_current.copy_structure str in
    let print =
      if !LiquidOptions.ocaml_syntax then
        Reason_toolchain.ML.print_implementation_with_comments
      else
        Reason_toolchain.RE.print_implementation_with_comments
    in
    print ppf (str, reason_comments);
    flush_str_formatter ()

end

module LiquidDebug = struct


  let rec bprint_contract_sig expand b indent { sig_name; entries_sig } =
    match sig_name with
    | Some s -> Printf.bprintf b "%s" s
    | None ->
      let indent2 = indent ^ "      " in
      Printf.bprintf b "%s(sig\n" indent;
      Printf.bprintf b "%stype storage\n" indent2;
      List.iter (fun e ->
          Printf.bprintf b "%sentry %s: (" indent2 e.entry_name;
          bprint_type_base expand b indent2 e.parameter;
          Printf.bprintf b " * storage) -> (operation list * storage)\n";
        ) entries_sig;
      Printf.bprintf b "%send)" indent

  and bprint_type_base expand b indent ty =
    let rec bprint_type b indent ty =
      match ty with
      | Tfail -> Printf.bprintf b "failure"
      | Tunit -> Printf.bprintf b "unit"
      | Tbool -> Printf.bprintf b "bool"
      | Tint -> Printf.bprintf b "int"
      | Tnat -> Printf.bprintf b "nat"
      | Ttez -> Printf.bprintf b "%s" (LiquidOptions.amount_type ())
      | Tstring -> Printf.bprintf b "string"
      | Tbytes -> Printf.bprintf b "bytes"
      | Ttimestamp  -> Printf.bprintf b "timestamp"
      | Tkey  -> Printf.bprintf b "key"
      | Tkey_hash  -> Printf.bprintf b "key_hash"
      | Tsignature  -> Printf.bprintf b "signature"
      | Toperation  -> Printf.bprintf b "operation"
      | Taddress  -> Printf.bprintf b "address"
      | Tchainid  -> Printf.bprintf b "chain_id"
      | Ttuple [] -> assert false
      | Ttuple (ty :: tys) ->
        Printf.bprintf b "(";
        bprint_type b "" ty;
        List.iter (fun ty ->
            Printf.bprintf b " * ";
            bprint_type b "" ty;
          ) tys;
        Printf.bprintf b ")";
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
      | Tsum (None, []) ->
        Printf.bprintf b "[]";
      | Tsum (None, (c, ty) :: rtys) ->
        Printf.bprintf b "[ %s of " c;
        bprint_type b "" ty;
        List.iter (fun (c, ty) ->
            Printf.bprintf b " | %s of " c;
            bprint_type b "" ty;
          ) rtys;
        Printf.bprintf b " ]";
      | Tsum (_, (c, ty) :: rtys) when expand ->
        Printf.bprintf b "%s of " c;
        bprint_type b "" ty;
        List.iter (fun (c, ty) ->
            Printf.bprintf b " | %s of " c;
            bprint_type b "" ty;
          ) rtys;
      | Tsum (Some name, _) ->
        Printf.bprintf b "%s" name;
      | Tcontract_handle (Some entry, ty) ->
        Printf.bprintf b "[%%handle %s : " entry;
        bprint_type b "" ty;
        Printf.bprintf b "]";
      | Tcontract_handle (None, ty) ->
        Printf.bprintf b "[%%handle ";
        bprint_type b "" ty;
        Printf.bprintf b "]";
      | Tcontract_view (v, t1, t2) ->
        Printf.bprintf b "[%%view %s : " v;
        bprint_type b "" t1;
        Printf.bprintf b " -> ";
        bprint_type b "" t2;
        Printf.bprintf b "]";
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
      | Tbigmap (ty1, ty2) ->
        Printf.bprintf b "(";
        bprint_type b "" ty1;
        Printf.bprintf b ", ";
        bprint_type b "" ty2;
        Printf.bprintf b ") big_map";
      | Tlambda (ty1, ty2, u) ->
        bprint_type b "" ty1;
        Printf.bprintf b " -> ";
        bprint_type b "" ty2;
      | Tclosure ((ty_arg, ty_env), ty_r, u) ->
        bprint_type b "" ty_arg;
        Printf.bprintf b " {";
        bprint_type b "" ty_env;
        Printf.bprintf b "}-> ";
        bprint_type b "" ty_r;
      | Tvar tvr ->
        Printf.bprintf b "'%s (" (Ref.get tvr).id;
        begin match (Ref.get tvr).tyo with
          | Some ty -> bprint_type b indent (LiquidTypes.expand ty)
          | None -> Printf.bprintf b "-" end;
        Printf.bprintf b ")"
      | Tpartial (Peqn _) -> Printf.bprintf b "peqn"
      | Tpartial (Ptup pl) ->
        Printf.bprintf b "(ptup:";
        List.iter (fun (n,t) ->
            let id = match t with Tvar tvr -> (Ref.get tvr).id | _ -> "-" in
            Printf.bprintf b " %d:%s" n id) pl;
        Printf.bprintf b ")";
      | Tpartial (Pmap _) -> Printf.bprintf b "pmap"
      | Tpartial (Pcont None) -> Printf.bprintf b "pcont none"
      | Tpartial (Pcont Some (e, ty)) ->
        Printf.bprintf b "pcont[%s : " e;
        bprint_type b indent (LiquidTypes.expand ty);
        Printf.bprintf b "]";
      | Tpartial (Ppar) -> Printf.bprintf b "ppar"
    in
    bprint_type b indent ty

  let rec bprint_type ?(expand=false) b indent ty =
    bprint_type_base expand b indent ty


  let bprint_type2 b indent ty =
    (* let set = ref StringSet.empty in *)
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
        Michelson.bprint_type_base Michelson.multi_line
          (* (fun _ b indent ty_name ty ->
           *   Printf.bprintf b "%s" ty_name;
           *   if not ( StringSet.mem ty_name !set ) then begin
           *       set := StringSet.add ty_name !set;
           *       todo := (Some ty_name, ty) :: !todo
           *     end
           * ) *)
          b indent ty [];
        Printf.bprintf b "\n";
        iter ()
    in
    iter ()

  let rec bprint_const bprint_code b indent cst =
    match cst with
    | CString s -> Printf.bprintf b "%S" s
    | CBytes s -> Printf.bprintf b "%s" s
    | CKey s -> Printf.bprintf b "%s" s
    | CKey_hash s -> Printf.bprintf b "%s" s
    | CContract (s, None) -> Printf.bprintf b "%s" s
    | CContract (s, Some e) -> Printf.bprintf b "%s%%%s" s e
    | CSignature s -> Printf.bprintf b "%s" s
    | CTez s -> Printf.bprintf b "%s%s" (LiquidNumber.liq_of_tez s)
                  (LiquidOptions.curreny ())
    | CInt n -> Printf.bprintf b "%s" (LiquidNumber.liq_of_integer n)
    | CNat n -> Printf.bprintf b "%sp" (LiquidNumber.liq_of_integer n)
    | CTimestamp s -> Printf.bprintf b "%s" s
    | CBool v -> Printf.bprintf b "%b" v
    | CUnit -> Printf.bprintf b "()"
    | CNone -> Printf.bprintf b "None"
    | CSome cst ->
      Printf.bprintf b "(Some ";
      bprint_const bprint_code b "" cst;
      Printf.bprintf b ")";
    | CLeft cst ->
      Printf.bprintf b "(Left ";
      bprint_const bprint_code b "" cst;
      Printf.bprintf b ")";
    | CRight cst ->
      Printf.bprintf b "(Right ";
      bprint_const bprint_code b "" cst;
      Printf.bprintf b ")";
    | CTuple [] -> assert false
    | CTuple (c :: cs) ->
      Printf.bprintf b "(";
      bprint_const bprint_code b "" c;
      List.iter (fun c ->
          Printf.bprintf b ", ";
          bprint_const bprint_code b "" c;
        ) cs;
      Printf.bprintf b ")";
    | CMap [] -> Printf.bprintf b "(Map [])";
    | CBigMap BMId id ->
      Printf.bprintf b "(BigMap %s)" (LiquidNumber.liq_of_integer id);
    | CBigMap BMList [] -> Printf.bprintf b "(BigMap [])";
    | CMap ((c1, c2) :: pairs) | CBigMap BMList ((c1, c2) :: pairs) ->
      let indent2 = indent ^ "      " in
      if String.length indent > 2 then Printf.bprintf b "\n%s" indent;
      Printf.bprintf b "(%s [" (match cst with
          | CMap _ -> "Map"
          | CBigMap _ -> "BigMap"
          | _ -> assert false);
      bprint_const bprint_code b indent c1;
      Printf.bprintf b ", ";
      bprint_const bprint_code b indent c2;
      List.iter (fun (c1, c2) ->
          Printf.bprintf b ";\n%s" indent2;
          bprint_const bprint_code b indent2 c1;
          Printf.bprintf b ", ";
          bprint_const bprint_code b indent2 c2;
        ) pairs;
      Printf.bprintf b "])";
    | CList [] -> Printf.bprintf b "[]";
    | CList (c :: csts) ->
      let indent2 = indent ^ " " in
      if String.length indent > 2 then Printf.bprintf b "\n%s" indent;
      Printf.bprintf b "[";
      bprint_const bprint_code b "" c;
      List.iter (fun c ->
          Printf.bprintf b ";\n%s" indent2;
          bprint_const bprint_code b indent2 c
        ) csts;
      Printf.bprintf b "]";
    | CSet [] -> Printf.bprintf b "(Set [])";
    | CSet (c :: csts) ->
      let indent2 = indent ^ "      " in
      if String.length indent > 2 then Printf.bprintf b "\n%s" indent;
      Printf.bprintf b "(Set [";
      bprint_const bprint_code b "" c;
      List.iter (fun c ->
          Printf.bprintf b ";\n%s" indent2;
          bprint_const bprint_code b indent2 c
        ) csts;
      Printf.bprintf b "])";
    | CConstr (c, cst) ->
      Printf.bprintf b "(%s " c;
      bprint_const bprint_code b "" cst;
      Printf.bprintf b ")";
    | CRecord labels ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      if String.length indent > 2 then Printf.bprintf b "\n%s" indent;
      Printf.bprintf b "{";
      List.iter (fun (label, cst) ->
          Printf.bprintf b "\n%s%s = " indent2 label;
          bprint_const bprint_code b indent4 cst;
          Printf.bprintf b ";";
        ) labels;
      Printf.bprintf b "\n%s}" indent
    | CLambda { arg_name; arg_ty; body; ret_ty } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s(fun ( %s : " indent arg_name.nname;
      bprint_type b indent2 arg_ty;
      Printf.bprintf b ") ->\n%s" indent2;
      bprint_code b indent4 body;
      Printf.bprintf b ")"

  let rec bprint_code_base bprint_code_rec ~debug b indent code =
    if debug && not (StringSet.is_empty code.bv) then begin
      Printf.bprintf b "\n%s(*\n" indent;
      (*        bprint_type b indent code.ty; *)
      Printf.bprintf b "%sbound:" indent;
      StringSet.iter (fun s -> Printf.bprintf b " %s" s) code.bv;
      Printf.bprintf b "\n%s*)" indent;
    end;

    match code.desc with
    | Let { bnd_var; inline; bnd_val; body } ->
      let indent2 = indent ^ "  " in
      Printf.bprintf b "\n%slet %s =" indent bnd_var.nname;
      bprint_code_rec ~debug b indent2 bnd_val;
      Printf.bprintf b "\n%s%sin" indent (match inline with
          | InForced -> "[@@inline] "
          | InDont -> "[@@noinline] "
          | InAuto -> "");
      bprint_code_rec ~debug b indent body
    | Const { ty ; const } ->
      Printf.bprintf b "\n%s" indent;
      bprint_const (bprint_code_base bprint_code_rec ~debug:false) b indent const;
    | Var name ->
      Printf.bprintf b " %s" name;
    | SetField { record; field; set_val } ->
      let indent2 = indent ^ "  " in
      Printf.bprintf b "\n%s(" indent;
      bprint_code_rec ~debug b indent2 record;
      Printf.bprintf b ".%s <-" field;
      bprint_code_rec ~debug b indent2 set_val;
      Printf.bprintf b ")";
    | Project { field; record } ->
      bprint_code_rec ~debug b indent record;
      Printf.bprintf b ".%s" field;
    | Failwith arg ->
      Printf.bprintf b "\n%sCurrent.failwith" indent;
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 arg;
    | Apply { prim; args } ->
      Printf.bprintf b "\n%s(%s" indent
        (LiquidTypes.string_of_primitive prim);
      let indent2 = indent ^ "  " in
      List.iter (fun exp ->
          Printf.bprintf b " ";
          bprint_code_rec ~debug b indent2 exp;
        ) args;
      Printf.bprintf b ")"
    | If { cond; ifthen; ifelse } ->
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
    | Transfer { dest; amount } ->
      Printf.bprintf b "\n%s(Account.transfer" indent;
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 dest;
      bprint_code_rec ~debug b indent2 amount;
      Printf.bprintf b ")"
    | Call { amount = Some _; entry = View _ }
    | Call { amount = None; entry = (Entry _ | NoEntry) } -> assert false
    | Call { contract ; amount = Some amount;
             entry = NoEntry; arg } ->
      Printf.bprintf b "\n%s(Contract.call" indent;
      (match contract with
      | DContract contract -> bprint_code_rec ~debug b indent contract
      | DSelf -> Printf.bprintf b "self"
      );
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 arg;
      bprint_code_rec ~debug b indent2 amount;
      Printf.bprintf b ")"
    | Call { contract; amount = Some amount;
             entry = Entry entry; arg } ->
      Printf.bprintf b "\n%s(" indent;
      (match contract with
      | DContract contract -> bprint_code_rec ~debug b indent contract
      | DSelf -> Printf.bprintf b "self"
      );
      Printf.bprintf b ".%s" entry;
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 arg;
      bprint_code_rec ~debug b indent2 amount;
      Printf.bprintf b ")"
    | Call { contract; amount = None;
             entry = View view; arg } ->
      Printf.bprintf b "\n%s(" indent;
      (match contract with
      | DContract contract -> bprint_code_rec ~debug b indent contract
      | DSelf -> Printf.bprintf b "self"
      );
      Printf.bprintf b ".%s" view;
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 arg;
      Printf.bprintf b ")"

    | Self { entry } ->
      Printf.bprintf b "\n%s[%%handle Self.%s]" indent entry;
    | SelfCall { amount; entry; arg } ->
      Printf.bprintf b "\n%s(Self.%s" indent entry;
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 arg;
      bprint_code_rec ~debug b indent2 amount;
      Printf.bprintf b ")"
    | MatchOption { arg; ifnone; some_name; ifsome } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%smatch " indent;
      bprint_code_rec ~debug b indent2 arg;
      Printf.bprintf b " with\n";
      Printf.bprintf b "\n%s| None ->\n" indent2;
      bprint_code_rec ~debug b indent4 ifnone;
      Printf.bprintf b "\n%s| Some %s ->\n" indent2 some_name.nname;
      bprint_code_rec ~debug b indent4 ifsome;
      ()
    | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%smatch%%nat " indent;
      bprint_code_rec ~debug b indent2 arg;
      Printf.bprintf b " with\n";
      Printf.bprintf b "\n%s| Plus %s ->\n" indent2 plus_name.nname;
      bprint_code_rec ~debug b indent4 ifplus;
      Printf.bprintf b "\n%s| Minus %s ->\n" indent2 minus_name.nname;
      bprint_code_rec ~debug b indent4 ifminus;
      ()
    | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%smatch " indent;
      bprint_code_rec ~debug b indent2 arg;
      Printf.bprintf b " with\n";
      Printf.bprintf b "\n%s| [] ->\n" indent2;
      bprint_code_rec ~debug b indent4 ifnil;
      Printf.bprintf b "\n%s| %s :: %s ->\n" indent2 head_name.nname tail_name.nname;
      bprint_code_rec ~debug b indent4 ifcons;
      ()
    | Loop { arg_name; body; arg } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%sLoop.loop (fun %s -> " indent arg_name.nname;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")\n%s" indent2;
      bprint_code_rec ~debug b indent2 arg;
      ()
    | LoopLeft { arg_name; body; arg; acc } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%sLoop.left (fun %s -> " indent arg_name.nname;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")\n%s" indent2;
      bprint_code_rec ~debug b indent2 arg;
      (match acc with
       | Some acc ->
         bprint_code_rec ~debug b indent2 acc
       | None -> ());
      ()
    | Fold { prim = (Prim_map_iter|Prim_set_iter|Prim_list_iter as prim);
             arg_name; body; arg } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s%s (fun %s -> "
        indent (LiquidTypes.string_of_fold_primitive prim) arg_name.nname;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")\n%s" indent2;
      bprint_code_rec ~debug b indent2 arg;
      ()
    | Fold { prim; arg_name; body; arg; acc } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s%s (fun %s -> "
        indent (LiquidTypes.string_of_fold_primitive prim) arg_name.nname;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")\n%s" indent2;
      bprint_code_rec ~debug b indent2 arg;
      bprint_code_rec ~debug b indent2 acc;
      ()
    | Map { prim; arg_name; body; arg }  ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s%s (fun %s -> "
        indent (LiquidTypes.string_of_map_primitive prim) arg_name.nname;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")\n%s" indent2;
      bprint_code_rec ~debug b indent2 arg;
      ()
    | MapFold { prim; arg_name; body; arg; acc }  ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s%s (fun %s -> "
        indent (LiquidTypes.string_of_map_fold_primitive prim) arg_name.nname;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")\n%s" indent2;
      bprint_code_rec ~debug b indent2 arg;
      bprint_code_rec ~debug b indent2 acc;
      ()
    | Closure { arg_name; arg_ty; body; ret_ty; call_env } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s{ env = " indent;
      List.iter (fun (s, e) ->
          Printf.bprintf b "\n%s%s ->" indent2 s;
          bprint_code_rec ~debug b indent4 e;
        ) call_env;
      Printf.bprintf b ";\n%s  f = (fun ( %s : " indent arg_name.nname ;
      bprint_type b indent2 arg_ty;
      Printf.bprintf b ") ->\n%s" indent2;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")\n%s}" indent
    | Lambda { arg_name; arg_ty; body; ret_ty } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s(fun ( %s : " indent arg_name.nname;
      bprint_type b indent2 arg_ty;
      Printf.bprintf b ") ->\n%s" indent2;
      bprint_code_rec ~debug b indent4 body;
      Printf.bprintf b ")"
    | Record fields ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%s{" indent;
      List.iter (fun (label, exp) ->
          Printf.bprintf b "\n%s%s = " indent2 label;
          bprint_code_rec ~debug b indent4 exp;
          Printf.bprintf b ";";
        ) fields;
      Printf.bprintf b "\n%s}" indent
    | Constructor { constr = Constr constr; arg } ->
      Printf.bprintf b "\n%s%s (" indent constr;
      bprint_code_rec ~debug b (indent ^ "  ") arg;
      Printf.bprintf b ")"
    | Constructor { constr = Left right_ty; arg } ->
      Printf.bprintf b "\n%s(Left " indent;
      bprint_code_rec ~debug b (indent ^ "  ") arg;
      Printf.bprintf b " : (_, ";
      bprint_type b (indent ^ "  ") right_ty;
      Printf.bprintf b ") variant)"
    | Constructor { constr = Right right_ty; arg } ->
      Printf.bprintf b "\n%s(Right " indent;
      bprint_code_rec ~debug b (indent ^ "  ") arg;
      Printf.bprintf b " : ( ";
      bprint_type b (indent ^ "  ") right_ty;
      Printf.bprintf b ", _) variant)"
    | MatchVariant { arg; cases } ->
      let indent2 = indent ^ "  " in
      let indent4 = indent2 ^ "  " in
      Printf.bprintf b "\n%smatch " indent;
      bprint_code_rec ~debug b indent2 arg;
      Printf.bprintf b " with\n";
      List.iter (function
          | PConstr (constr, vars), e ->
            Printf.bprintf b "\n%s| %s (%s) ->\n" indent2 constr
              (String.concat ", " vars);
            bprint_code_rec ~debug b indent4 e;
          | PAny, e ->
            Printf.bprintf b "\n%s| _ ->\n" indent2;
            bprint_code_rec ~debug b indent4 e;
        ) cases;
      ()
    | CreateContract { args; contract } ->
      Printf.bprintf b "\n%s(Contract.create" indent;
      let indent2 = indent ^ "  " in
      List.iter (fun exp ->
          bprint_code_rec ~debug b indent2 exp
        ) args;
      Printf.bprintf b "\n%s(contract %s)" indent contract.contract_name;
    | HandleAt { arg; entry; entry_param } ->
      let kind, entry = match entry with
        | NoEntry -> "entry", "default"
        | Entry e -> "entry", e
        | View v -> "view", v in
      Printf.bprintf b "\n%s[%%%%handle val%%%s %s : " indent kind entry;
      bprint_type b (indent ^ "  ") entry_param;
      Printf.bprintf b " -> _ ]"
    | Unpack { arg; ty } ->
      Printf.bprintf b "\n%s(Bytes.unpack" indent;
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 arg;
      Printf.bprintf b " : ";
      bprint_type b (indent ^ "  ") (Toption ty);
      Printf.bprintf b ")"
    | TypeAnnot { e; ty } ->
      Printf.bprintf b "\n%s(" indent;
      let indent2 = indent ^ "  " in
      bprint_code_rec ~debug b indent2 e;
      Printf.bprintf b " : ";
      bprint_type b (indent ^ "  ") ty;
      Printf.bprintf b ")"
    | Type ty ->
      Printf.bprintf b "\n%s" indent;
      bprint_type b (indent ^ "  ") ty

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

  let bprint_entry bprint_code ~debug b indent storage_ty entry =
    let indent2 = indent ^ "    " in
    Printf.bprintf b "let%%entry %s\n" entry.entry_sig.entry_name;
    (* Printf.bprintf b "    (amount: tez)\n"; *)
    Printf.bprintf b "    (%s/2: " entry.entry_sig.parameter_name;
    bprint_type b indent2 entry.entry_sig.parameter;
    Printf.bprintf b ")\n";
    Printf.bprintf b "    (%s/1: " entry.entry_sig.storage_name;
    bprint_type b indent2 storage_ty;
    Printf.bprintf b ")";
    begin match entry.fee_code with
      | None -> ()
      | Some fee_code ->
        Printf.bprintf b "\n    [@@fee \n";
        bprint_code ~debug b (indent ^ "      ") fee_code;
        Printf.bprintf b "\n    ]\n";
    end;
    Printf.bprintf b " = \n";
   bprint_code ~debug b indent entry.code

  let bprint_contract bprint_code ~debug b indent contract =
    let storage_ty = contract.storage in
    List.iter (fun entry ->
        bprint_entry bprint_code ~debug b indent storage_ty entry;
        Printf.bprintf b "\n";
      ) contract.entries

  let string_of_type = to_string bprint_type
  let string_of_const c = to_string (bprint_const (bprint_code ~debug:false)) c
  let string_of_code ?(debug=false) code =
    to_string (bprint_code ~debug) code
  let string_of_code_types ?(debug=false) code =
    to_string (bprint_code_types ~debug) code
  let string_of_contract ?(debug=false) cmd =
    to_string (bprint_contract bprint_code ~debug) cmd
  let string_of_contract_types ?(debug=false) cmd =
    to_string (bprint_contract bprint_code_types ~debug) cmd

end

module Liquid = struct

  let string_of_type ty =
    try
      LiquidToParsetree.convert_type ty
      |> Syntax.string_of_core_type
    with _ ->
      LiquidDebug.string_of_type ty

  let string_of_const c =
    try
      LiquidToParsetree.convert_const c
      |> Syntax.string_of_expression
    with _ ->
      LiquidDebug.string_of_const c

  let string_of_contract c =
    try
      Syntax.string_of_structure (LiquidToParsetree.structure_of_contract c) []
    with _ ->
      LiquidDebug.string_of_contract c

  let string_of_code c =
    try
      LiquidToParsetree.convert_code c
      |> Syntax.string_of_expression
    with _ ->
      LiquidDebug.string_of_code c

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
  | N_TRANSFER -> "N_TRANSFER"
  | N_CALL -> "N_CALL"
  | N_CONST (ty, cst) -> "N_CONST " (* ^ Liquid.string_of_const cst *)
  | N_PRIM string ->
    Printf.sprintf "N_PRIM %s" string
  | N_FAILWITH -> "N_FAILWITH"
  | N_LOOP _ -> "N_LOOP"
  | N_LOOP_BEGIN _ -> "N_LOOP_BEGIN"
  | N_LOOP_END _ -> "N_LOOP_END"
  | N_ARG (_,int) -> Printf.sprintf "N_ARG %d" int
  | N_LOOP_RESULT (_,_, int) -> Printf.sprintf "N_LOOP_RESULT %d" int
  | N_FOLD _ -> "N_FOLD"
  | N_FOLD_BEGIN _ -> "N_FOLD_BEGIN"
  | N_FOLD_END _ -> "N_FOLD_END"
  | N_FOLD_RESULT (_,_, int) -> Printf.sprintf "N_FOLD_RESULT %d" int
  | N_MAP _ -> "N_MAP"
  | N_MAP_BEGIN _ -> "N_MAP_BEGIN"
  | N_MAP_END _ -> "N_MAP_END"
  | N_MAP_RESULT (_,_, int) -> Printf.sprintf "N_MAP_RESULT %d" int
  | N_LAMBDA _ -> "N_LAMBDA"
  | N_LAMBDA_BEGIN -> "N_LAMBDA_BEGIN"
  | N_LAMBDA_END _ -> "N_LAMBDA_END"
  | N_UNKNOWN s -> Printf.sprintf "N_UNKNOWN %S" s
  | N_END -> "N_END"
  | N_LEFT _ -> "N_LEFT"
  | N_RIGHT _ -> "N_RIGHT"
  | N_CONTRACT (None, _) -> "N_CONTRACT"
  | N_CONTRACT (Some e, _) -> Printf.sprintf "N_CONTRACT %s" e
  | N_UNPACK _ -> "N_UNPACK"
  | N_ABS -> "N_ABS"
  | N_CREATE_CONTRACT _ -> "N_CREATE_CONTRACT"
  | N_RECORD fields -> "N_RECORD_" ^ (String.concat "_" fields)
  | N_PROJ f -> "N_PROJ " ^ f
  | N_CONSTR c -> "N_CONSTR " ^ c
  | N_SETFIELD f -> "N_SETFIELD " ^ f
  | N_RESULT (_, i) -> Printf.sprintf "N_RESULT %d" i
  | N_LOOP_LEFT _ -> "N_LOOP_LEFT"
  | N_LOOP_LEFT_BEGIN _ -> "N_LOOP_LEFT_BEGIN"
  | N_LOOP_LEFT_END _ -> "N_LOOP_LEFT_END"
  | N_LOOP_LEFT_RESULT _ -> "N_LOOP_LEFT_RESULT"
  | N_SELF None -> "N_SELF"
  | N_SELF Some e -> Printf.sprintf "N_SELF %s" e
