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
open LiquidMichelineTypes
open Dune_Network_Lib.Stdlib
open Dune_Network_Lib.Micheline
open Dune_Network_Lib.Data_encoding
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

let rec get_type ty = (* expand ty *) match ty with
  | Tvar { contents = { contents = { tyo = Some ty }}} -> get_type ty
  | _ -> ty

let rec get_tyo ty = match ty with
  | None -> None
  | Some ty -> Some (get_type ty)

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
  let s = LiquidMisc.string_replace s '.' '_' in
  if List.mem s reserved_keywords || has_reserved_prefix s then
    s ^ "_"
  else if String.length s > 0 then
    match s.[0] with
    | 'A' .. 'Z' when not allow_capital -> "_" ^ s
    | '0' .. '9' -> "_" ^ s
    | _ -> s
  else s

let type_name_of_annots ?(allow_capital=false) annots =
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
  | Found s -> Some (sanitize_name ~allow_capital s)


let type_constr_or_label_of_annots ~allow_capital ?(keep_empty=false) ?(no_ignore=false) annots =
  if not no_ignore && !LiquidOptions.ignore_annots then []
  else
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

let entryname_of_annots annots =
  let cstrs =
    type_constr_or_label_of_annots ~allow_capital:true ~keep_empty:false ~no_ignore:true annots in
  match cstrs with
  | [] -> None
  | x :: _ -> Some (String.uncapitalize_ascii x)


let add_generalize_to_env =
  let cpt = ref 0 in
  fun name ty env ->
    match StringMap.find_opt name env.types with
    | None ->
      env.types <- StringMap.add name ([ty], !cpt) env.types;
      incr cpt
    | Some _ when name = "_entries" ->
      env.types <- StringMap.add (name ^ string_of_int !cpt) ([ty], !cpt) env.types;
      incr cpt
    | Some (l, i) ->
      List.iter (LiquidInfer.generalize ty) l;
      env.types <- StringMap.add name (ty :: l, i) env.types

let rec convert_type ?(parameter=false) env expr =
  let name = match expr with
    | Prim(_, _, _, a) -> begin match type_name_of_annots a with
        | Some "storage" -> Some "storage_"
        | n -> n
      end
    | _ -> None
  in
  let ty = match expr with
    | Prim(_, "unit", [], _debug) -> Tunit
    | Prim(_, "timestamp", [], _debug) -> Ttimestamp
    | Prim(_, ("mutez" | "mudun"), [], _debug) -> Ttez
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
            let ty = Trecord (name, type_labels ~gen:false env expr) in
            let ty_gen = Trecord (name, type_labels ~gen:true env expr) in (* copy *)
            add_generalize_to_env name ty_gen env;
            ty
          with Exit -> Ttuple [convert_type env x; convert_type env y]
      end

    | Prim(_, "or", [x;y], _debug) ->
      begin match name with
        (* | None when not parameter -> Tor (convert_type env x, convert_type env y) *)
        | _ ->
          let is_variant = match name with None -> true | Some _ -> false in
          try
            let ty = Tsum (name, type_constrs ~gen:false ~is_variant env expr) in
            let ty_gen = Tsum (name, type_constrs ~gen:true ~is_variant env expr) in
            (match name with
             | None -> ()
             | Some name -> add_generalize_to_env name ty_gen env);
            ty
          with Exit -> Tor (convert_type env x, convert_type env y)
      end

    | Prim(_, "contract", [x], annots) ->
      let parameter = convert_type env x in
      Tcontract_handle (None, parameter)

    | Prim(_, "lambda", [x;y], _debug) ->
      Tlambda
        (convert_type env x,
         convert_type env y,
         dont_uncurry () (* no uncurrying while decompiling *))
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
  (* Wrap in type variable with type eq for generalization *)
  LiquidInfer.wrap_tvar ty


and type_components ~allow_capital ~gen env t =
  let mk_ty_comp ty =
    if gen && env.generalize_types then begin
      ignore (convert_type env ty); (* for side effects in env *)
      LiquidInfer.fresh_tvar ()
    end
    else
      convert_type env ty
  in
  match t with
  | Prim(_, _, [x;y], annots) ->
    let label_of_annot = function
      | Prim(_, "big_map", _, a) -> type_name_of_annots ~allow_capital a
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
        [x_label, mk_ty_comp x; y_label, mk_ty_comp y]
      | Some x_label, None ->
        (x_label, mk_ty_comp x) :: type_components ~allow_capital ~gen env y
    end
  | _ -> raise Exit

and type_constrs ~gen ~is_variant env t =
  let constrs = type_components ~allow_capital:true ~gen env t in
  if is_variant then
    List.map (fun (c, ty) -> ("`" ^ c, ty)) constrs
  else constrs

and type_labels ~gen env t =
  type_components ~allow_capital:false ~gen env t


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

let rec find nodes ?annots name =
  match nodes with
  | [] -> raise (Missing_program_field name)
  | Prim(_, name_maybe, [ v ], a) :: nodes ->
    if name_maybe = name then
      match annots with
      | None -> v, a
      | Some a' when a = a' -> v, a
      | Some _ ->
        find nodes ?annots name
    else find nodes ?annots name
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


let rec convert_const env ?ty expr =
  let ty = get_tyo ty in
  let loc = match expr with
    | Int (i, _) | String (i, _) | Bytes (i, _)
    | Prim (i, _, _, _) | Seq (i, _) ->
      loc_of_int env i in
  let convert_const env ?ty e =
    fst (convert_const env ?ty e) in
  let c = match expr with
    | Int (_, n) ->
      begin match ty with
        | Some Tnat -> CNat (LiquidNumber.integer_of_mic n)
        | Some Tint | None -> CInt (LiquidNumber.integer_of_mic n)
        | Some Ttez -> CTez (LiquidNumber.tez_of_mic_mutez n)
        | Some (Tbigmap (_k, _v)) ->
          CBigMap (BMId (LiquidNumber.integer_of_mic n))
        | Some ty -> wrong_type env expr ty
      end

    | String (_, s) ->
      begin match ty with
        | Some Ttez ->
          CTez (LiquidNumber.tez_of_mic_mutez (Z.of_string s))
        | Some Ttimestamp ->
          CTimestamp (ISO8601.of_string s)
        | Some Tkey -> CKey s
        | Some Tkey_hash -> CKey_hash s
        | Some Taddress | Some Tcontract_handle _ ->
          (match String.split_on_char '%' s with
           | [s] -> CContract (s, None)
           | [s; e] -> CContract (s, Some e)
           | _ -> assert false)
        | Some Tsignature -> CSignature s
        | Some Tstring | None -> CString s
        | Some ty -> wrong_type env expr ty
      end

    | Bytes (_, s) ->
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
        | Some (Taddress | Tcontract_handle _) ->
          let c = MBytes.sub s 0 22 in
          let e = MBytes.sub s 22 (MBytes.length s - 22) in
          let e =
            if Bytes.equal e Bytes.empty then None
            else Some (MBytes.to_string e) in
          CContract (to_hex c, e)
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
          CBigMap (BMList (List.map (function
              | Prim(_, "Elt", [k;e], _debug) ->
                convert_const env ~ty:ty_k k, convert_const env ~ty:ty_e e
              | expr ->
                unknown_expr env "convert_const big map element" expr
            ) elems))
        | Some (Tlambda (arg_ty, ret_ty, _)) ->
          CLambda { arg_name = { nname = "_" ; nloc = loc };
                    recursive = None;
                    arg_ty; ret_ty;
                    body = convert_code env expr }
        | None ->
          (try CList (List.map (convert_const env) elems)
           with _ ->
             (* maybe it's a lambda *)
             CLambda { arg_name = { nname = "_" ; nloc = loc };
                       recursive = None;
                       arg_ty = LiquidInfer.fresh_tvar ();
                       ret_ty = LiquidInfer.fresh_tvar ();
                       body = convert_code env expr })
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
  in
  (c, loc)

and convert_code env expr =
  match expr with
  | Seq (index, exprs) ->
    mic_loc env index []
      (SEQ (List.map (convert_code env) exprs))
  | Prim (index, "RENAME", [], annots) ->
    mic_loc env index annots (RENAME (name_of_annots annots)) (* TODO remove *)
  | Prim(index, "DUP", [], annot) ->
    mic_loc env index annot (DUP 1)
  | Prim(index, "DROP", [], annot) ->
    mic_loc env index annot (DROP 1)
  | Prim(index, "DROP", [ Int (_, i) ], annot) ->
    mic_loc env index annot (DROP (Z.to_int i))
  | Prim(index, "DIP", [ arg ], annot) ->
    mic_loc env index annot (DIP (1, convert_code env arg))
  | Prim(index, "DIP", [ Int (_, i); arg ], annot) ->
    mic_loc env index annot (DIP (Z.to_int i, convert_code env arg))
  | Prim(index, "DIG", [ Int (_, i) ], annot) ->
    mic_loc env index annot (DIG (Z.to_int i))
  | Prim(index, "DUG", [ Int (_, i) ], annot) ->
    mic_loc env index annot (DUG (Z.to_int i))
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
  | Prim(index, "CHAIN_ID", [], annot) ->
    mic_loc env index annot (CHAIN_ID)
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
    let entry = entryname_of_annots annot in
    mic_loc env index annot (SELF entry)
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
    begin match get_type ty, convert_const env ~ty cst with
      | Tnat, (CInt n, _) ->
        mic_loc env index annot (PUSH (Tnat, CNat n))
      | ty, (cst, _) ->
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
    let entry = entryname_of_annots annot in
    mic_loc env index annot (CONTRACT (entry, convert_type env ty))
  | Prim(index, "UNPACK", [ty], annot) ->
    mic_loc env index annot
      (UNPACK (convert_type env ty))
  | Prim(index, "CONS", [], annot) ->
    mic_loc env index annot (CONS)
  | Prim(index, "LOOP", [loop], annot) ->
    mic_loc env index annot
      (LOOP (convert_code env loop))
  | Prim(index, "LOOP_LEFT", [loop], annot) ->
    mic_loc env index annot
      (LOOP_LEFT (convert_code env loop))
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
  | Prim(index, "CREATE_CONTRACT", [Seq (_, contract)], annot) ->
    let contract = convert_raw_contract env contract in
    mic_loc env index annot (CREATE_CONTRACT contract)
  | Prim(index, "IMPLICIT_ACCOUNT", [], annot) ->
    mic_loc env index annot (IMPLICIT_ACCOUNT)
  | Prim(index, "SET_DELEGATE", [], annot) ->
    mic_loc env index annot (SET_DELEGATE)
  | Prim(index, "ADDRESS", [], annot) ->
    mic_loc env index annot (ADDRESS)

  | Prim(index, "BLOCK_LEVEL", [], annot) ->
    mic_loc env index annot (BLOCK_LEVEL)
  | Prim(index, "COLLECT_CALL", [], annot) ->
    mic_loc env index annot (COLLECT_CALL)
  | Prim(index, "IS_IMPLICIT", [], annot) ->
    mic_loc env index annot (IS_IMPLICIT)
  | Prim(index, "GET_BALANCE", [], annot) ->
    mic_loc env index annot (GET_BALANCE)

  | Prim(index, "EMPTY_BIG_MAP", [ k; v ], annot) ->
    let k = convert_type env k in
    let v = convert_type env v in
    mic_loc env index annot (EMPTY_BIG_MAP (k, v))

  | _ -> unknown_expr env "convert_code" expr

and root_name_of_param p annots =
  match entryname_of_annots annots with
  | Some _ as e -> e
  | None -> match p with
    | Prim(_, "or", [_; _], annots) -> entryname_of_annots annots
    | _ -> None

and convert_raw_contract env c =
  let param_node, param_annots = find c "parameter" in
  let mic_parameter = convert_type env ~parameter:true param_node in
  let mic_root = root_name_of_param param_node param_annots in
  let mic_storage = convert_type env (find c "storage" |> fst) in
  let mic_code = convert_code env (find c "code" ~annots:[] |> fst) in
  let mic_fee_code =
    try
      Some (convert_code env (find c "code" ~annots:["@fee"] |> fst))
    with Missing_program_field _ -> None in
  { mic_storage; mic_parameter; mic_code; mic_fee_code; mic_root }

let convert_contract env c =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Parse Michelson contract@.";
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
      let env = { (LiquidMichelineTypes.empty_env filename) with
                  loc_table = convert_loc_table filename loc_tables;
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
      let env = { (LiquidMichelineTypes.empty_env filename) with
                  loc_table = convert_loc_table filename [loc_table];
                } in
      Some (node, env)

let convert_env env =
  let ty_env = LiquidFromParsetree.initial_env env.filename in
  let new_types_map =
    StringMap.map (fun (tys, new_id) ->
        let ty = match tys with
          | [] -> assert false
          | ty :: _ -> LiquidInfer.instantiate (LiquidTypes.free_tvars ty, ty)
        in
        begin match ty with
          | Trecord (name, labels) ->
            List.iteri (fun i (label, l_ty) ->
                ty_env.labels <- StringMap.add label (name, i) ty_env.labels;
              ) labels;
          | Tsum (Some name, constrs) ->
            List.iteri (fun i (constr, c_ty) ->
                ty_env.constrs <-
                  StringMap.add constr (name, i) ty_env.constrs;
              ) constrs;
          | _ -> ()
        end;
        let params = LiquidTypes.free_tvars ty |> StringSet.elements in
        (fun pvals ->
           let subst = LiquidInfer.make_subst params pvals in
           LiquidInfer.instantiate_to subst ty
           ), new_id
    ) env.types in
    ty_env.types <- StringMap.union (fun _ t1 _ -> Some t1) new_types_map ty_env.types;
  ty_env.contract_types <-
    List.fold_right (fun (n, c_sig) -> StringMap.add n c_sig)
      env.contract_types ty_env.contract_types;
  ty_env


let infos_env env =
  let types =
    [] |> StringMap.fold (fun n (tys, i) acc ->
        match tys with
        | [] -> assert false
        | ty :: _ -> ((n, ty), i) :: acc
      ) env.types
    |> List.sort (fun (_, i1) (_, i2) -> Pervasives.compare i1 i2)
    |> List.map fst in
  env.annoted, env.type_annots, types
