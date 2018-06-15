(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes
open LiquidTezosTypes
open Michelson_Tezos
open Micheline

exception Missing_program_field of string

let liquid_loc_of_script_loc f { Micheline_parser.start; stop } =
  { loc_file = f;
    loc_pos = Some (
        (start.Micheline_parser.line, start.Micheline_parser.column),
        (stop.Micheline_parser.line, stop.Micheline_parser.column))
  }

let loc_error filename =
  let open Micheline_parser in
  function
  | Invalid_utf8_sequence (pt, _)
  | Unexpected_character (pt, _)
  | Undefined_escape_sequence (pt, _)
  | Missing_break_after_number pt ->
    liquid_loc_of_script_loc filename { start = pt; stop = pt }
  | Unterminated_string loc
  | Unterminated_integer loc
  | Unterminated_comment loc
  | Unclosed { loc }
  | Unexpected { loc }
  | Extra { loc } ->
    liquid_loc_of_script_loc filename loc
  | Misaligned n ->
    let loc = Micheline.location n in
    liquid_loc_of_script_loc filename loc
  | _ -> LiquidLoc.loc_in_file filename

let string_of_token = function
  | Micheline_parser.Open_paren
  | Micheline_parser.Close_paren -> "parenthesis"
  | Micheline_parser.Open_brace
  | Micheline_parser.Close_brace -> "curly brace"
  | Micheline_parser.String _ -> "string constant"
  | Micheline_parser.Int _ -> "integer constant"
  | Micheline_parser.Ident _ -> "identifier"
  | Micheline_parser.Annot _ -> "annotation"
  | Micheline_parser.Comment _
  | Micheline_parser.Eol_comment _ -> "comment"
  | Micheline_parser.Semi -> "semi colon"

let msg_error =
  let open Micheline_parser in
  function
  | Invalid_utf8_sequence (point, msg) ->
    Printf.sprintf "Invalid UTF-8 sequence %S" msg
  | Unexpected_character (point, msg) ->
    Printf.sprintf "Unexpected character %s" msg
  | Undefined_escape_sequence (point, msg) ->
    Printf.sprintf "Undefined escape character %S" msg
  | Missing_break_after_number point -> "Missing break"
  | Unterminated_string loc -> "Unterminated string"
  | Unterminated_integer loc -> "Unterminated integer"
  | Unterminated_comment loc -> "Unterminated comment"
  | Unclosed { loc ; token } ->
    Printf.sprintf "Unclosed %s" (string_of_token token)
  | Unexpected { loc ; token } ->
    Printf.sprintf "Unexpected %s" (string_of_token token)
  | Extra { loc ; token } ->
    Printf.sprintf "Extra %s" (string_of_token token)
  | Misaligned node -> "Misaligned expression"
  | Empty -> "Empty expression"
  | _ -> ""

let error_to_liqerror filename e = {
  err_loc = loc_error filename e;
  err_msg = msg_error e
}

let loc_of_int env index =
  try IntMap.find index env.loc_table
  with Not_found -> LiquidLoc.noloc

let unknown_expr env msg expr =
  let loc = loc_of_int env (Micheline.location expr) in
  LiquidLoc.raise_error ~loc "in %s, %s" msg
    (match expr with
     | Seq (_loc, _exprs, _debug) -> "unknwon sequence"
     | String (_loc, s) ->
       Printf.sprintf "unknwon string %S" s
     | Int (_loc, i) ->
       Printf.sprintf "unknown integer %s" (Z.to_string i)
     | Prim(i, s, args, _debug) ->
       Printf.sprintf "unknown primitive %S with %d arguments"
         s (List.length args)
    )

let wrong_type env expr ty =
  let loc = loc_of_int env (Micheline.location expr) in
  LiquidLoc.raise_error ~loc "type of %s is not convertible with %s"
    (match expr with
     | Seq (_loc, _exprs, _debug) -> "sequence"
     | String (_loc, s) ->
       Printf.sprintf "string %S" s
     | Int (_loc, i) ->
       Printf.sprintf "integer %s" (Z.to_string i)
     | Prim(i, s, args, _debug) ->
       Printf.sprintf "primitive %S" s
    )
    (LiquidPrinter.Liquid.string_of_type ty)

let rec convert_const env ?ty expr =
  match expr with
  | Int (_loc, n) ->
    begin match ty with
      | Some Tnat -> CNat (LiquidPrinter.integer_of_mic n)
      | Some Tint | None -> CInt (LiquidPrinter.integer_of_mic n)
      | Some Ttez -> CTez (LiquidPrinter.tez_of_mic_mutez n)
      | Some ty -> wrong_type env expr ty
    end

  | String (_loc, s) ->
    begin match ty with
      | Some Ttez ->
        CTez (LiquidPrinter.tez_of_mic_mutez (Z.of_string s))
      | Some Ttimestamp -> CTimestamp s
      | Some Tkey -> CKey s
      | Some Tkey_hash -> CKey_hash s
      | Some Tcontract _ -> CContract s
      | Some Taddress -> CAddress s
      | Some Tsignature -> CSignature s
      | Some Tstring | None -> CString s
      | Some ty -> wrong_type env expr ty
    end

  | Prim(_, "Unit", [], _debug) -> CUnit
  | Prim(_, "True", [], _debug) -> CBool true
  | Prim(_, "False", [], _debug) -> CBool false

  | Prim(_, "None", [], _debug) -> CNone
  | Prim(_, "Some", [x], _debug) ->
    begin match ty with
      | None -> CSome (convert_const env x)
      | Some (Toption ty) -> CSome (convert_const env ~ty x)
      | Some ty -> wrong_type env expr ty
    end

  | Prim(_, "Left", [x], _debug) ->
    begin match ty with
      | Some (Tsum (tname, (c, ty):: _)) ->
        CConstr (c, convert_const env ~ty x)
      | Some (Tor (ty, _)) -> CLeft (convert_const env ~ty x)
      | None -> CLeft (convert_const env x)
      | Some ty -> wrong_type env expr ty
    end

  | Prim(_, "Right", [x], _debug) ->
    begin match ty with
      | Some (Tsum (tname, [_; (c, ty)])) ->
        CConstr (c, convert_const env ~ty x)
      | Some (Tsum (tname, _ :: rconstrs)) ->
        convert_const env ~ty:(Tsum(tname, rconstrs)) x
      | Some (Tor (_, ty)) -> CRight (convert_const env ~ty x)
      | None -> CRight (convert_const env x)
      | Some ty -> wrong_type env expr ty
    end

  | Prim(_, "Pair", [x;y], _debug) ->
    begin match ty with
      | Some (Trecord (tname, [fx, tyx; fy, tyy])) ->
        CRecord [fx, convert_const env ~ty:tyx x;
                 fy, convert_const env ~ty:tyy y]
      | Some (Trecord (tname, (f, ty):: rfields) as ty') ->
        begin match convert_const env ~ty:(Trecord (tname, rfields)) y with
          | CRecord fields -> CRecord ((f, convert_const env ~ty x) :: fields)
          | _ -> wrong_type env expr ty'
        end
      | Some (Ttuple [tyx;tyy]) ->
        CTuple [convert_const env ~ty:tyx x; convert_const env ~ty:tyy y]
      | None ->
        CTuple [convert_const env x; convert_const env y]
      | Some (Ttuple (ty :: r) as ty') ->
        begin match convert_const env ~ty:(Ttuple r) y with
          | CTuple l -> CTuple (convert_const env ~ty x :: l)
          | _ -> wrong_type env expr ty'
        end
      | Some ty -> wrong_type env expr ty
    end

  | Seq(_, elems, _debug) ->
    begin match ty with
      | Some (Tlist ty) ->
        CList (List.map (convert_const ~ty env) elems)
      | Some (Tset ty) ->
        CSet (List.map (convert_const ~ty env) elems)
      | Some (Tmap (ty_k, ty_e)) ->
        CMap (List.map (function
            | Prim(_, "Elt", [k;e], _debug) ->
                convert_const env ~ty:ty_k k, convert_const env ~ty:ty_e e
            | expr ->
              unknown_expr env "convert_const map element" expr
          ) elems)
      | Some (Tbigmap (ty_k, ty_e)) ->
        CBigMap (List.map (function
            | Prim(_, "Elt", [k;e], _debug) ->
                convert_const env ~ty:ty_k k, convert_const env ~ty:ty_e e
            | expr ->
              unknown_expr env "convert_const big map element" expr
          ) elems)
      | None ->
        CList (List.map (convert_const env) elems)
      | Some ty ->  wrong_type env expr ty
    end

  | Prim(_, "Elt", [k;e], _debug) ->
    begin match ty with
      | Some (Tmap (ty_k, ty_e)) ->
        CMap [convert_const env ~ty:ty_k k, convert_const env ~ty:ty_e e]
      | None ->
        CMap [convert_const env k, convert_const env e]
      | Some ty ->
        wrong_type env expr ty
    end

  | _ -> unknown_expr env "convert_const" expr

let name_of_annot name =
  Scanf.sscanf name "@%s" (fun s -> s)

let rec convert_type env expr =
  let name = match expr with
    | Prim(_, _, _, Some a) -> Some (name_of_annot a)
    | _ -> None
  in
  let ty = match expr with
    | Prim(_, "unit", [], _debug) -> Tunit
    | Prim(_, "timestamp", [], _debug) -> Ttimestamp
    | Prim(_, "mutez", [], _debug) -> Ttez
    | Prim(_, "int", [], _debug) -> Tint
    | Prim(_, "nat", [], _debug) -> Tnat
    | Prim(_, "bool", [], _debug) -> Tbool
    | Prim(_, "key", [], _debug) -> Tkey
    | Prim(_, "key_hash", [], _debug) -> Tkey_hash
    | Prim(_, "signature", [], _debug) -> Tsignature
    | Prim(_, "string", [], _debug) -> Tstring
    | Prim(_, "operation", [], _debug) -> Toperation
    | Prim(_, "address", [], _debug) -> Taddress
    (* | Prim(_, "pair", [
     *     Prim(_, _, _, Some x_field) as x;
     *     Prim(_, _, _, Some y_field) as y;
     *   ], _debug) ->
     *   let x_field = (name_of_annot x_field) in
     *   let y_field = (name_of_annot y_field) in
     *   let rname = match name with
     *     | Some n -> n
     *     | None -> String.concat "_" [x_field; y_field] in
     *   Trecord (rname, [(x_field, convert_type env x);
     *                    (y_field, convert_type env y)]) *)
    | Prim(_, "pair", [x;y], _debug) -> Ttuple [convert_type env x;
                                                convert_type env y]

    (* | Prim(_, "or", [
     *     Prim(_, _, _, Some x_constr) as x;
     *     Prim(_, _, _, Some y_constr) as y;
     *   ], _debug) ->
     *   let x_constr = String.capitalize_ascii @@ name_of_annot x_constr in
     *   let y_constr = String.capitalize_ascii @@ name_of_annot y_constr in
     *   let sname = match name with
     *     | Some n -> n
     *     | None -> String.concat "_" [x_constr; y_constr] in
     *   Tsum (sname, [(x_constr, convert_type env x);
     *                 (y_constr, convert_type env y)]) *)
    | Prim(_, "or", [x;y], _debug) -> Tor (convert_type env x,
                                           convert_type env y)
    | Prim(_, "contract", [x], _debug) -> Tcontract (convert_type env x)
    | Prim(_, "lambda", [x;y], _debug) -> Tlambda
                                            (convert_type env x,
                                             convert_type env y)
    | Prim(_, "map", [x;y], _debug) ->
      Tmap (convert_type env x, convert_type env y)
    | Prim(_, "big_map", [x;y], _debug) ->
      Tbigmap (convert_type env x, convert_type env y)
    | Prim(_, "set", [x], _debug) -> Tset (convert_type env x)
    | Prim(_, "list", [x], _debug) -> Tlist (convert_type env x)
    | Prim(_, "option", [x], _debug) -> Toption (convert_type env x)
    | _ -> unknown_expr env "convert_type" expr
  in
  begin match name with
    | None -> ()
    | Some name -> Hashtbl.add env.type_annots ty name
  end;
  ty

(*
let is_liq_annot name =
  try Scanf.sscanf name "@liq_" true
  with _ -> false

let liq_annot name =
  Scanf.sscanf name "@liq_%s" (fun s -> s)
*)


let mic_loc env index annot ins =
  let loc_name = match annot with
    | Some annot ->
      env.annoted <- true;
      Some (name_of_annot annot)
    | _ -> None
  in
  let loc = loc_of_int env index in
  { ins; loc; loc_name }

let rec find nodes name =
  match nodes with
  | [] -> raise (Missing_program_field name)
  | Prim(_, name_maybe, [ v ], _) :: nodes ->
     if name_maybe = name then v
     else find nodes name
  | _ -> raise (Missing_program_field name)

let rec expand expr =
  match Michelson_v1_macros.expand expr with
  | Error _ -> invalid_arg "expand"
  | Ok r -> match r with
    | Seq (loc, items, annot) ->
      Seq (loc, List.map expand items, annot)
    | Prim (loc, name, args, annot) ->
      Prim (loc, name, List.map expand args, annot)
    | Int _ | String _ as atom -> atom


let rec convert_code env expr =
  match expr with
  | Seq (index, [], Some name) ->
    mic_loc env index (Some name) (ANNOT (name_of_annot name)) (* TODO remove *)
  | Seq (index, exprs, annot) ->
    mic_loc env index annot
      (SEQ (List.map (convert_code env) exprs))
  | Prim(index, "DUP", [], annot) ->
    mic_loc env index annot (DUP 1)
  | Prim(index, "DROP", [], annot) ->
    mic_loc env index annot (DROP)
  | Prim(index, "DIP", [ arg ], annot) ->
    mic_loc env index annot (DIP (1, convert_code env arg))
  | Prim(index, "CAR", [], annot) ->
    mic_loc env index annot (CAR)
  | Prim(index, "CDR", [], annot) ->
    mic_loc env index annot (CDR)
  | Prim(index, "SWAP", [], annot) ->
    mic_loc env index annot (SWAP)
  | Prim(index, "IF", [x;y], annot) ->
    mic_loc env index annot
      (IF (convert_code env x, convert_code env y))
  | Prim(index, "IF_NONE", [x;y], annot) ->
    mic_loc env index annot
      (IF_NONE (convert_code env x, convert_code env y))
  | Prim(index, "IF_LEFT", [x;y], annot) ->
    mic_loc env index annot
      (IF_LEFT (convert_code env x, convert_code env y))
  | Prim(index, "IF_CONS", [x;y], annot) ->
    mic_loc env index annot
      (IF_CONS (convert_code env x, convert_code env y))
  | Prim(index, "NOW", [], annot) ->
    mic_loc env index annot (NOW)
  | Prim(index, "PAIR", [], annot) ->
    mic_loc env index annot (PAIR)
  | Prim(index, "BALANCE", [], annot) ->
    mic_loc env index annot (BALANCE)
  | Prim(index, "SUB", [], annot) ->
    mic_loc env index annot (SUB)
  | Prim(index, "ADD", [], annot) ->
    mic_loc env index annot (ADD)
  | Prim(index, "MUL", [], annot) ->
    mic_loc env index annot (MUL)
  | Prim(index, "NEQ", [], annot) ->
    mic_loc env index annot (NEQ)
  | Prim(index, "EQ", [], annot) ->
    mic_loc env index annot (EQ)
  | Prim(index, "LT", [], annot) ->
    mic_loc env index annot (LT)
  | Prim(index, "LE", [], annot) ->
    mic_loc env index annot (LE)
  | Prim(index, "GT", [], annot) ->
    mic_loc env index annot (GT)
  | Prim(index, "GE", [], annot) ->
    mic_loc env index annot (GE)
  | Prim(index, "GET", [], annot) ->
    mic_loc env index annot (GET)
  | Prim(index, "UPDATE", [], annot) ->
    mic_loc env index annot (UPDATE)
  | Prim(index, "MEM", [], annot) ->
    mic_loc env index annot (MEM)
  | Prim(index, "SOME", [], annot) ->
    mic_loc env index annot (SOME)
  | Prim(index, "MANAGER", [], annot) ->
    mic_loc env index annot (MANAGER)
  | Prim(index, "SOURCE", [], annot) ->
    mic_loc env index annot (SOURCE)
  | Prim(index, "SELF", [], annot) ->
    mic_loc env index annot (SELF)
  | Prim(index, "OR", [], annot) ->
    mic_loc env index annot (OR)
  | Prim(index, "LAMBDA", [ty1; ty2; expr], annot) ->
    mic_loc env index annot
      (LAMBDA (convert_type env ty1, convert_type env ty2,
               convert_code env expr))
  | Prim(index, "COMPARE", [], annot) ->
    mic_loc env index annot (COMPARE)
  | Prim(index, "FAIL", [], annot) ->
    mic_loc env index annot (FAIL None)
  | Prim(index, "UNIT", [], annot) ->
    mic_loc env index annot (PUSH (Tunit, CUnit))
  | Prim(index, "TRANSFER_TOKENS", [], annot) ->
    mic_loc env index annot (TRANSFER_TOKENS)
  | Prim(index, "PUSH", [ ty; cst ], annot) ->
    let ty = convert_type env ty in
    begin match ty, convert_const env ~ty cst with
      | Tnat, CInt n ->
        mic_loc env index annot (PUSH (Tnat, CNat n))
      | ty, cst ->
        mic_loc env index annot (PUSH (ty, cst))
    end
  | Prim(index, "H", [], annot) ->
    mic_loc env index annot (H)
  | Prim(index, "HASH_KEY", [], annot) ->
    mic_loc env index annot (HASH_KEY)
  | Prim(index, "CHECK_SIGNATURE", [], annot) ->
    mic_loc env index annot (CHECK_SIGNATURE)
  | Prim(index, "CONCAT", [], annot) ->
    mic_loc env index annot (CONCAT)
  | Prim(index, "EDIV", [], annot) ->
    mic_loc env index annot (EDIV)
  | Prim(index, "EXEC", [], annot) ->
    mic_loc env index annot (EXEC)
  | Prim(index, "MOD", [], annot) ->
    mic_loc env index annot (MOD)
  | Prim(index, "DIV", [], annot) ->
    mic_loc env index annot (DIV)
  | Prim(index, "NEG", [], annot) ->
    mic_loc env index annot (NEG)
  | Prim(index, "AMOUNT", [], annot) ->
    mic_loc env index annot (AMOUNT)
  | Prim(index, "NIL", [ty], annot) ->
    mic_loc env index annot
      (PUSH (Tlist (convert_type env ty), CList []))
  | Prim(index, "EMPTY_SET", [ty], annot) ->
    mic_loc env index annot
      (PUSH (Tset (convert_type env ty), CSet []))
  | Prim(index, "EMPTY_MAP", [ty1; ty2], annot) ->
    mic_loc env index annot
      (PUSH (Tmap (convert_type env ty1, convert_type env ty2), CMap []))
  | Prim(index, "NONE", [ty], annot) ->
    mic_loc env index annot
      (PUSH (Toption (convert_type env ty), CNone))
  | Prim(index, "LEFT", [ty], annot) ->
    mic_loc env index annot
      (LEFT (convert_type env ty))
  | Prim(index, "CONTRACT", [ty], annot) ->
    mic_loc env index annot
      (CONTRACT (convert_type env ty))
  | Prim(index, "CONS", [], annot) ->
    mic_loc env index annot (CONS)
  | Prim(index, "LOOP", [loop], annot) ->
    mic_loc env index annot
      (LOOP (convert_code env loop))
  | Prim(index, "ITER", [body], annot) ->
    mic_loc env index annot
      (ITER (convert_code env body))
  | Prim(index, "MAP", [body], annot) ->
    mic_loc env index annot
      (MAP (convert_code env body))
  | Prim(index, "RIGHT", [ty], annot) ->
    mic_loc env index annot
      (RIGHT (convert_type env ty))
  | Prim(index, "INT", [], annot) ->
    mic_loc env index annot (INT)
  | Prim(index, "SIZE", [], annot) ->
    mic_loc env index annot (SIZE)
  | Prim(index, "AND", [], annot) ->
    mic_loc env index annot (AND)
  | Prim(index, "XOR", [], annot) ->
    mic_loc env index annot (XOR)
  | Prim(index, "ABS", [], annot) ->
    mic_loc env index annot (ABS)
  | Prim(index, "NOT", [], annot) ->
    mic_loc env index annot (NOT)
  | Prim(index, "STEPS_TO_QUOTA", [], annot) ->
    mic_loc env index annot (STEPS_TO_QUOTA)
  | Prim(index, "CREATE_ACCOUNT", [], annot) ->
    mic_loc env index annot (CREATE_ACCOUNT)
  | Prim(index, "CREATE_CONTRACT", [Seq (_, contract, _)], annot) ->
    let contract = convert_raw_contract env contract in
    mic_loc env index annot (CREATE_CONTRACT contract)
  | Prim(index, "IMPLICIT_ACCOUNT", [], annot) ->
    mic_loc env index annot (IMPLICIT_ACCOUNT)
  | Prim(index, "SET_DELEGATE", [], annot) ->
    mic_loc env index annot (SET_DELEGATE)
  | Prim(index, "ADDRESS", [], annot) ->
    mic_loc env index annot (ADDRESS)

  | _ -> unknown_expr env "convert_code" expr

and convert_raw_contract env c =
  let parameter = convert_type env (find c "parameter") in
  let storage = convert_type env (find c "storage") in
  let code = convert_code env (find c "code") in
  { contract_sig = { storage; parameter }; code }

let convert_contract env c =
  let c =
    List.map (fun c ->
        let c = Micheline.inject_locations (fun i -> i) c in
        expand c) c in
  let contract = convert_raw_contract env c in
  contract, env.annoted, env.type_annots

let convert_const_type env c ty =
  let c = Micheline.inject_locations (fun i -> i) c in
  convert_const env ~ty c

let convert_const_notype env c =
  let c = Micheline.inject_locations (fun i -> i) c in
  convert_const env c


let convert_loc_table f loc_tables =
  List.fold_left (
    List.fold_left (fun acc (i, p) ->
        IntMap.add i (liquid_loc_of_script_loc f p) acc
      )
  ) IntMap.empty loc_tables

let contract_of_string filename s =
  let tokens, errors = Micheline_parser.tokenize s in
  match errors with
  | error :: _ ->
    raise (LiquidError (error_to_liqerror filename error))
  | [] ->
     let nodes, errors = Micheline_parser.parse_toplevel tokens in
     match errors with
     | error :: _ ->
       raise (LiquidError (error_to_liqerror filename error))
     | [] ->
        let nodes = List.map Micheline.extract_locations nodes in
        let (nodes, loc_tables) = List.split nodes in
        let env = { filename;
                    loc_table = convert_loc_table filename loc_tables;
                    type_annots = Hashtbl.create 17;
                    annoted = false;
                  } in
        Some (nodes, env)

let const_of_string filename s =
  let tokens, errors = Micheline_parser.tokenize s in
  match errors with
  | error :: _ ->
    raise (LiquidError (error_to_liqerror filename error))
  | [] ->
     let node, errors = Micheline_parser.parse_expression tokens in
     match errors with
     | error :: _ ->
       raise (LiquidError (error_to_liqerror filename error))
     | [] ->
        let node, loc_table = Micheline.extract_locations node in
        let env = { filename;
                    loc_table = convert_loc_table filename [loc_table];
                    type_annots = Hashtbl.create 17;
                    annoted = false;
                  } in
        Some (node, env)
