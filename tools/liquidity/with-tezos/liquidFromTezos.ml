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
  | Micheline_parser.Bytes _ -> "bytes constant"
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
     | Seq (_loc, _exprs) -> "unknwon sequence"
     | String (_loc, s) ->
       Printf.sprintf "unknwon string %S" s
     | Bytes (_loc, s) ->
       Printf.sprintf "unknwon bytes %S" (Hex.show (MBytes.to_hex s))
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
     | Seq (_loc, _exprs) -> "sequence"
     | String (_loc, s) ->
       Printf.sprintf "string %S" s
     | Bytes (_loc, s) ->
       Printf.sprintf "bytes %S" (Hex.show (MBytes.to_hex s))
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

  | Bytes (_loc, s) ->
    let to_hex s = "0x" ^ Hex.show (MBytes.to_hex s) in
    begin match ty with
      | None | Some Tbytes ->
        CBytes (to_hex s)
      | Some Tkey ->
        (* CKey Ed25519.Public_key.(MBytes.to_string s |> of_bytes |> to_b58check) *)
        CKey (to_hex s)
      | Some Tkey_hash ->
        (* CKey_hash Ed25519.Public_key_hash.(of_bytes_exn s |> to_b58check) *)
        CKey_hash (to_hex s)
      | Some Tsignature ->
        (* CSignature Ed25519.Signature.(to_b58check s) *)
        CSignature (to_hex s)
      | Some Taddress ->
        CAddress (to_hex s)
      | Some (Tcontract _) ->
        CContract (to_hex s)
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

  | Seq(_, elems) ->
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

let name_of_annots annots =
  let exception Found of string in
  try
    List.iter (fun a ->
      try raise (Found (Scanf.sscanf a "@%s" (fun s -> s)))
      with Scanf.Scan_failure _ | End_of_file -> ()
    ) annots;
    None
  with
  | Found ("" | "%" | "%%") -> None
  | Found s -> Some s

let sanitize_name ~allow_capital s =
  if List.mem s reserved_keywords || has_reserved_prefix s then
    s ^ "_"
  else if String.length s > 0 then
    match s.[0] with
    | 'A' .. 'Z' when not allow_capital -> "_" ^ s
    | '0' .. '9' -> "_" ^ s
    | _ -> s
  else s

let type_name_of_annots annots =
  let exception Found of string in
  try
    List.iter (fun a ->
      try raise (Found (Scanf.sscanf a ":%s" (fun s -> s)))
      (* with Scanf.Scan_failure _ | End_of_file ->
       * try raise (Found (Scanf.sscanf a "%%%s" (fun s -> s))) *)
      with Scanf.Scan_failure _ | End_of_file -> ()
    ) annots;
    None
  with
  | Found ("" | "%" | "@") -> None
  | Found s -> Some (sanitize_name ~allow_capital:false s)


let type_constr_or_label_of_annots ~allow_capital ?(keep_empty=false) annots =
  let exception Found of string in
  List.fold_left (fun acc a ->
      try Scanf.sscanf a "%%%s"
            (function
              | "" when keep_empty -> "" :: acc
              | "" | "%" | "@" -> acc
              | s -> sanitize_name ~allow_capital s :: acc)
      with Scanf.Scan_failure _ | End_of_file -> acc
    ) [] annots
  |> List.rev

let type_constr_of_annots annots =
  type_constr_or_label_of_annots ~allow_capital:true ~keep_empty:true annots

let type_label_of_annots annots =
  type_constr_or_label_of_annots ~allow_capital:false annots

let rec convert_type env expr =
  let name = match expr with
    | Prim(_, _, _, a) -> type_name_of_annots a
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
    | Prim(_, "bytes", [], _debug) -> Tbytes
    | Prim(_, "operation", [], _debug) -> Toperation
    | Prim(_, "address", [], _debug) -> Taddress

    | Prim(_, "pair", [x;y], _debug) ->
      begin match name with
        | None -> Ttuple [convert_type env x; convert_type env y]
        | Some name ->
          try
            let ty = Trecord (name, type_labels env expr) in
            if not @@ List.mem_assoc name env.types then
              env.types <- (name, ty) :: env.types;
            ty
          with Exit -> Ttuple [convert_type env x; convert_type env y]
      end

    | Prim(_, "or", [x;y], _debug) ->
      begin match name with
        | None -> Tor (convert_type env x, convert_type env y)
        | Some name ->
          try
            let ty = Tsum (name, type_constrs env expr) in
            if not @@ List.mem_assoc name env.types then
              env.types <- (name, ty) :: env.types;
            ty
          with Exit -> Tor (convert_type env x, convert_type env y)
      end

    | Prim(_, "contract", [x], _debug) ->
      let parameter = convert_type env x in
      let contract_sig = {
        sig_name = None;
        entries_sig = [{
          entry_name = "main";
          parameter_name = "parameter";
          storage_name = "storage";
          parameter;
        }]
      } in
      Tcontract contract_sig
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


and type_components ~allow_capital env t =
  match t with
  | Prim(_, _, [x;y], annots) ->
    let label_of_annot = function
      | Prim(_, _, _, a) ->
        (match type_constr_or_label_of_annots ~allow_capital a with
         | [l] -> Some l
         | _ -> None)
      | _ -> None in
    begin
      let x_label = label_of_annot x in
      let y_label = label_of_annot y in
      match x_label, y_label with
      | None, _ -> raise Exit
      | Some x_label, Some y_label ->
        [x_label, convert_type env x; y_label, convert_type env y]
      | Some x_label, None ->
        (x_label, convert_type env x) :: type_components ~allow_capital env y
    end
  | _ -> raise Exit

and type_constrs env t = type_components ~allow_capital:true env t
and type_labels env t = type_components ~allow_capital:false env t


(*
let is_liq_annot name =
  try Scanf.sscanf name "@liq_" true
  with _ -> false

let liq_annot name =
  Scanf.sscanf name "@liq_%s" (fun s -> s)
*)


let mic_loc env index annots ins =
  let loc_name = name_of_annots annots in
  begin match loc_name with
    | Some _ -> env.annoted <- true;
    | None -> ()
  end;
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
    | Seq (loc, items) ->
      Seq (loc, List.map expand items)
    | Prim (loc, name, args, annot) ->
      Prim (loc, name, List.map expand args, annot)
    | Int _ | String _ | Bytes _ as atom -> atom


let rec convert_code env expr =
  match expr with
  | Seq (index, exprs) ->
    mic_loc env index []
      (SEQ (List.map (convert_code env) exprs))
  | Prim (index, "RENAME", [], annots) ->
    mic_loc env index annots (RENAME (name_of_annots annots)) (* TODO remove *)
  | Prim(index, "DUP", [], annot) ->
    mic_loc env index annot (DUP 1)
  | Prim(index, "DROP", [], annot) ->
    mic_loc env index annot (DROP)
  | Prim(index, "DIP", [ arg ], annot) ->
    mic_loc env index annot (DIP (1, convert_code env arg))
  | Prim(index, "CAR", [], annot) ->
    begin match type_label_of_annots annot with
      | [f] -> mic_loc env index annot (CAR (Some f))
      | _ -> mic_loc env index annot (CAR None)
    end
  | Prim(index, "CDR", [], annot) ->
    begin match type_label_of_annots annot with
      | [f] -> mic_loc env index annot (CDR (Some f))
      | _ -> mic_loc env index annot (CDR None)
    end
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
    begin match type_label_of_annots annot with
      | [x] -> mic_loc env index annot (RECORD (x, None))
      | [x; y] -> mic_loc env index annot (RECORD (x, Some y))
      | _ -> mic_loc env index annot (PAIR)
    end
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
  | Prim(index, "SOURCE", [], annot) ->
    mic_loc env index annot (SOURCE)
  | Prim(index, "SENDER", [], annot) ->
    mic_loc env index annot (SENDER)
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
  | Prim(index, "FAILWITH", [], annot) ->
    mic_loc env index annot (FAILWITH)
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
  | Prim(index, "PACK", [], annot) ->
    mic_loc env index annot (PACK)
  | Prim(index, "BLAKE2B", [], annot) ->
    mic_loc env index annot (BLAKE2B)
  | Prim(index, "SHA256", [], annot) ->
    mic_loc env index annot (SHA256)
  | Prim(index, "SHA512", [], annot) ->
    mic_loc env index annot (SHA512)
  | Prim(index, "HASH_KEY", [], annot) ->
    mic_loc env index annot (HASH_KEY)
  | Prim(index, "CHECK_SIGNATURE", [], annot) ->
    mic_loc env index annot (CHECK_SIGNATURE)
  | Prim(index, "CONCAT", [], annot) ->
    mic_loc env index annot (CONCAT)
  | Prim(index, "SLICE", [], annot) ->
    mic_loc env index annot (SLICE)
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
    let ty = convert_type env ty in
    begin match type_constr_of_annots annot with
      | c :: _ -> mic_loc env index annot (LEFT (ty, Some c))
      | _ -> mic_loc env index annot (LEFT (ty, None))
    end
  | Prim(index, "RIGHT", [ty], annot) ->
    let ty = convert_type env ty in
    begin match type_constr_of_annots annot with
      | _ :: c :: _ ->
        mic_loc env index annot (RIGHT (ty, Some c))
      | _ -> mic_loc env index annot (RIGHT (ty, None))
    end
  | Prim(index, "CONTRACT", [ty], annot) ->
    mic_loc env index annot
      (CONTRACT (convert_type env ty))
  | Prim(index, "UNPACK", [ty], annot) ->
    mic_loc env index annot
      (UNPACK (convert_type env ty))
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
  | Prim(index, "ISNAT", [], annot) ->
    mic_loc env index annot (ISNAT)
  | Prim(index, "NOT", [], annot) ->
    mic_loc env index annot (NOT)
  | Prim(index, "LSL", [], annot) ->
    mic_loc env index annot (LSL)
  | Prim(index, "LSR", [], annot) ->
    mic_loc env index annot (LSR)
  | Prim(index, "STEPS_TO_QUOTA", [], annot) ->
    mic_loc env index annot (STEPS_TO_QUOTA)
  | Prim(index, "CREATE_ACCOUNT", [], annot) ->
    mic_loc env index annot (CREATE_ACCOUNT)
  | Prim(index, "CREATE_CONTRACT", [Seq (_, contract)], annot) ->
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
  let mic_parameter = convert_type env (find c "parameter") in
  let mic_storage = convert_type env (find c "storage") in
  let mic_code = convert_code env (find c "code") in
  { mic_storage; mic_parameter; mic_code }

let convert_contract env c =
  let c =
    List.map (fun c ->
        let c = Micheline.inject_locations (fun i -> i) c in
        expand c) c in
  let contract = convert_raw_contract env c in
  contract

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
                    types = [];
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
                    types = [];
                    annoted = false;
                  } in
        Some (node, env)

let convert_env env =
  let ty_env = LiquidFromOCaml.initial_env env.filename in
  let types = List.rev env.types in
  List.iter (fun (_, ty) -> match ty with
      | Trecord (name, labels) ->
        List.iteri (fun i (label, l_ty) ->
            ty_env.labels <- StringMap.add label (name, i, l_ty) ty_env.labels;
          ) labels;
      | Tsum (name, constrs) ->
        List.iter (fun (constr, c_ty) ->
            ty_env.constrs <-
              StringMap.add constr (name, c_ty) ty_env.constrs;
          ) constrs;
      | _ -> ()
    ) types;
  ty_env.types <-
    List.fold_left (fun acc (n, ty) -> StringMap.add n ty acc)
      ty_env.types types;
  ty_env


let infos_env env = env.annoted, env.type_annots, List.rev env.types
