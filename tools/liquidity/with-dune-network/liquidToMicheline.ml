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
open Dune_Network_Lib
open Micheline

type annot =
  | Eannot of string
  | Tannot of string
  | Fannot of string
  | Nannot of string

type loc_table = (int * (LiquidTypes.location * string option)) list

type michelson_code = (unit,string) Micheline.node

let ii ~loc ins = { ins; loc; loc_name  = None }

let loc_of_many (l : loc_michelson list) = match l, List.rev l with
  | [], _ | _, [] -> LiquidLoc.noloc
  | first :: _, last :: _ -> LiquidLoc.merge first.loc last.loc

let drop_bq l =
  if l = "" then l
  else match l.[0] with
    | '`' -> String.sub l 1 (String.length l - 1)
    | _ -> l

let convert_annot = function
  | Tannot l -> ":" ^ l
  | Fannot l | Eannot l -> "%" ^ drop_bq l
  | Nannot l -> "@" ^ l

let convert_annots annots =
  List.fold_left (fun acc a -> match a with
      | Eannot _ -> convert_annot a :: acc
      | _ ->
        if !LiquidOptions.no_annot then acc else convert_annot a :: acc)
    [] annots
  |> List.rev

let parse_annot x =
  let l = String.length x in
  if l = 0 then None
  else
    let n = String.sub x 1 (l - 1) in
    match x.[0] with
    | ':' -> Some (Tannot n)
    | '%' -> Some (Fannot n)
    | '@' -> Some (Nannot n)
    | _ -> None

let parse_annots annots =
  List.fold_left (fun acc a -> match parse_annot a with
      | Some a -> a :: acc
      | None -> acc) [] annots |> List.rev

let prim ~loc ?(fields=[]) name args var_name =
  let annots = List.map (fun f -> Fannot f) fields in
  let annots = match var_name with
    | Some s -> (Nannot s) :: annots
    | None -> annots
  in
  Micheline.Prim(loc, name, args, convert_annots annots)

let seq ~loc exprs =
  Micheline.Seq(loc, exprs)

let prim_type ~loc ?(annots=[]) name args =
  Micheline.Prim(loc, name, args, convert_annots annots)

let int ~loc n =
  Micheline.Int (loc, LiquidNumber.(mic_of_integer @@ integer_of_int n))

let rec convert_type ~loc expr =
  match expr with
  | Tunit -> prim_type ~loc "unit" []
  | Ttimestamp -> prim_type ~loc "timestamp" []
  | Ttez -> prim_type ~loc (LiquidOptions.mu_amount_type ()) []
  | Tint -> prim_type ~loc "int" []
  | Tnat -> prim_type ~loc "nat" []
  | Tbool -> prim_type ~loc "bool" []
  | Tkey -> prim_type ~loc "key" []
  | Tkey_hash -> prim_type ~loc "key_hash" []
  | Tsignature -> prim_type ~loc "signature" []
  | Tstring -> prim_type ~loc "string" []
  | Tbytes -> prim_type ~loc "bytes" []
  | Toperation -> prim_type ~loc "operation" []
  | Taddress -> prim_type ~loc "address" []
  | Tchainid -> prim_type ~loc "chain_id" []
  | Ttuple [x] -> assert false
  | Ttuple [] -> assert false
  | Ttuple [x;y] ->
    prim_type ~loc "pair" [convert_type ~loc x; convert_type ~loc y]
  | Ttuple (x :: tys) ->
    prim_type ~loc "pair" [convert_type ~loc x; convert_type ~loc (Ttuple tys)]
  | Tor (x,y) -> prim_type ~loc "or" [convert_type ~loc x; convert_type ~loc y]
  | Tcontract (e, parameter) ->
    prim_type ~loc "contract" [convert_type ~loc parameter]
  | Tlambda (x,y, _) ->
    prim_type ~loc "lambda" [convert_type ~loc x;
                             convert_type ~loc y]
  | Tclosure ((x,e),r, u) ->
    convert_type ~loc (Ttuple [Tlambda (Ttuple [x; e], r, u); e ]);
  | Tmap (x,y) -> prim_type ~loc "map" [convert_type ~loc x;convert_type ~loc y]
  | Tbigmap (x,y) ->
    prim_type ~loc "big_map" [convert_type ~loc x;convert_type ~loc y]
  | Tset x -> prim_type ~loc "set" [convert_type ~loc x]
  | Tlist x -> prim_type ~loc "list" [convert_type ~loc x]
  | Toption x -> prim_type ~loc "option" [convert_type ~loc x]
  | Trecord (name, labels) -> convert_record_type ~loc name labels
  | Tsum (name, constrs) -> convert_sum_type ~loc name constrs
  | Tfail -> convert_type ~loc Tunit (* use unit for failures *)
  | Tvar _ | Tpartial _ -> assert false

and convert_record_type ~loc name labels =
  convert_composed_type "pair" ~loc (Some name) labels

and convert_sum_type ~loc name constrs =
  convert_composed_type "or" ~loc name constrs

and convert_composed_type ty_c ~loc ?(parameter=false) name labels =
  let annots = match name with
    | None -> []
    | Some name -> [Tannot name] in
  match labels with
  | [] -> assert false
  | [l, ty] ->
    begin match convert_type ~loc ty with
      | Micheline.Prim(loc, "big_map", args, _annots) ->
        prim_type ~loc "big_map" args ~annots:[Tannot l]
      | Micheline.Prim(loc, name, args, annots) ->
        prim_type ~loc name args ~annots:(parse_annots annots @ [
            if parameter then Eannot l else Fannot l
          ])
      | _ -> assert false
    end
  | [lb, (Tbigmap _ as ty_b); lr, ty_r] ->
    (* workaround for { lb : _ big_map; lr : _ } => pair *)
    let ty_b = convert_type ~loc ty_b in
    let ty_r = convert_type ~loc ty_r in
    prim_type ~loc ~annots ty_c [ty_b; ty_r]
  | (l, ty) :: labels ->
    let ty = match convert_type ~loc ty with
      | Micheline.Prim(loc, "big_map", args, annots) ->
        prim_type ~loc "big_map" args ~annots:[Tannot l]
      | Micheline.Prim(loc, name, args, annots) ->
        prim_type ~loc name args ~annots:(parse_annots annots @ [
            if parameter then Eannot l else Fannot l
          ])
      | _ -> assert false in
    prim_type ~loc ~annots ty_c
      [ty; convert_composed_type ty_c ~loc ~parameter None labels]

let rec convert_const ~loc expand (expr : loc_michelson const) =
  let bytes_of_hex s =
    `Hex (String.sub s 2 (String.length s - 2))
    |> MBytes.of_hex in
  match expr with
  | CInt n -> Micheline.Int (loc, LiquidNumber.mic_of_integer n)
  | CString s -> Micheline.String (loc, s)
  | CBytes s -> Micheline.Bytes (loc, bytes_of_hex s)
  | CUnit -> Micheline.Prim(loc, "Unit", [], [])
  | CBool true -> Micheline.Prim(loc, "True", [], [])
  | CBool false -> Micheline.Prim(loc, "False", [], [])
  | CNone -> Micheline.Prim(loc, "None", [], [])

  | CSome x -> Micheline.Prim(loc, "Some", [convert_const ~loc expand x], [])
  | CLeft x -> Micheline.Prim(loc, "Left", [convert_const ~loc expand x], [])
  | CRight x -> Micheline.Prim(loc, "Right", [convert_const ~loc expand x], [])

  | CTuple [] -> assert false
  | CTuple [_] -> assert false
  | CTuple [x;y] ->
    Micheline.Prim(loc, "Pair", [convert_const ~loc expand x;
                                 convert_const ~loc expand y], [])
  | CTuple (x :: y) ->
    Micheline.Prim(loc, "Pair", [convert_const ~loc expand x;
                                 convert_const ~loc expand (CTuple y)], [])

  | CList args | CSet args ->
    Micheline.Seq(loc, List.map (convert_const ~loc expand) args)

  | CMap args | CBigMap BMList args ->
    Micheline.Seq(loc,
                  List.map (fun (x,y) ->
                      Micheline.Prim(loc, "Elt", [convert_const ~loc expand x;
                                                  convert_const ~loc expand y], []
                                    ))
                    args)
  | CBigMap BMId n ->
    Micheline.Int (loc, LiquidNumber.mic_of_integer n)
  | CNat n -> Micheline.Int (loc, LiquidNumber.mic_of_integer n)
  | CTez n -> Micheline.Int (loc, LiquidNumber.mic_mutez_of_tez n)
           (*
  | CTez tez
    |CKey _|
   | CSignature _|CLeft _|CRight _)
            *)
  | CTimestamp s -> Micheline.String (loc, s)
  | CKey s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CKey s -> Micheline.String (loc, s)
  | CKey_hash s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CKey_hash s -> Micheline.String (loc, s)
  | CAddress s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CAddress s -> Micheline.String (loc, s)
  | CContract s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CContract s -> Micheline.String (loc, s)
  | CSignature s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CSignature s -> Micheline.String (loc, s)

  | CRecord fields ->
    convert_const ~loc expand (CTuple (List.map snd fields))

  | CLambda lam ->
    convert_code expand lam.body

  | _ ->
    LiquidLoc.raise_error ~loc:(fst loc) "to-micheline: unimplemented const:\n%s%!"
      (LiquidPrinter.Michelson.string_of_loc_michelson_const expr)

and convert_code expand expr =
  let name = expr.loc_name in
  let ii = ii ~loc:expr.loc in
  let seq = seq ~loc:(expr.loc, None) in
  let prim = prim ~loc:(expr.loc, None) in
  let int = int ~loc:(expr.loc, None)  in
  let convert_type ty = convert_type ~loc:(expr.loc, None) ty in
  let convert_const c = convert_const ~loc:(expr.loc, None) expand c in
  match expr.ins with
  | RENAME a -> prim "RENAME" [] a
  | SEQ exprs -> seq (List.map (convert_code expand) exprs)

  | FAILWITH -> prim "FAILWITH" [] None

  | DROP 1 -> prim "DROP" [] None
  | DROP n -> prim "DROP" [int n] None
  | DIP (0, arg) -> convert_code expand arg
  | DIP (1, arg) -> prim "DIP" [ convert_code expand arg ] None
  | DIP (n, arg) -> prim "DIP" [ int n; convert_code expand arg ] None
  | DIG 1 -> convert_code expand { expr with ins = SWAP }
  | DIG n -> prim "DIG" [int n] None
  | DUG n -> prim "DUG" [int n] None
  | CAR None -> prim "CAR" []  name
  | CAR (Some field) -> prim "CAR" [] ~fields:[field] name
  | CDR None -> prim "CDR" []  name
  | CDR (Some field) -> prim "CDR" [] ~fields:[field] name
  | SWAP -> prim "SWAP" [] None
  | IF (x,y) ->
    prim "IF" [convert_code expand x; convert_code expand y] name
  | IF_NONE (x,y) ->
    prim "IF_NONE" [convert_code expand x; convert_code expand y] name
  | IF_LEFT (x,y) ->
    prim "IF_LEFT" [convert_code expand x; convert_code expand y] name
  | IF_CONS (x,y) ->
    prim "IF_CONS" [convert_code expand x; convert_code expand y] name
  | NOW -> prim "NOW" [] name
  | PAIR -> prim "PAIR" [] name
  | RECORD (f1, None) -> prim "PAIR" [] ~fields:[f1] name
  | RECORD (f1, Some f2) -> prim "PAIR" [] ~fields:[f1; f2] name
  | BALANCE -> prim "BALANCE" [] name
  | SUB -> prim "SUB" [] name
  | ADD -> prim "ADD" [] name
  | MUL -> prim "MUL" [] name
  | NEQ -> prim "NEQ" [] name
  | EQ -> prim "EQ" [] name
  | LT -> prim "LT" [] name
  | LE -> prim "LE" [] name
  | GT -> prim "GT" [] name
  | GE -> prim "GE" [] name
  | GET -> prim "GET" [] name
  | UPDATE -> prim "UPDATE" [] name
  | MEM -> prim "MEM" [] name
  | SOME -> prim "SOME" [] name
  | ADDRESS -> prim "ADDRESS" [] name
  | SOURCE -> prim "SOURCE" [] name
  | SENDER -> prim "SENDER" [] name
  | OR -> prim "OR" [] name
  | LAMBDA (ty1, ty2, expr) ->
    prim "LAMBDA" [convert_type ty1; convert_type ty2; convert_code expand expr] name
  | COMPARE -> prim "COMPARE" [] name
  | PUSH (Tunit, CUnit) -> prim "UNIT" [] name
  | PUSH (Tlist ty, CList []) -> prim "NIL" [convert_type ty] name
  | PUSH (Toption ty, CNone) -> prim "NONE" [convert_type ty] name
  | TRANSFER_TOKENS -> prim "TRANSFER_TOKENS" [] name
  | PUSH (ty, cst) -> prim "PUSH" [ convert_type ty;
                                    convert_const cst ] name
  | PACK -> prim "PACK" [] name
  | BLAKE2B -> prim "BLAKE2B" [] name
  | SHA256 -> prim "SHA256" [] name
  | SHA512 -> prim "SHA512" [] name
  | HASH_KEY -> prim "HASH_KEY" [] name
  | CHECK_SIGNATURE -> prim "CHECK_SIGNATURE" [] name
  | CONCAT -> prim "CONCAT" [] name
  | SLICE -> prim "SLICE" [] name
  | EDIV -> prim "EDIV" [] name
  | EXEC -> prim "EXEC" [] name
  | MOD -> prim "MOD" [] name
  | DIV -> prim "DIV" [] name
  | AMOUNT -> prim "AMOUNT" [] name
  | CHAIN_ID -> prim "CHAIN_ID" [] name
                   (*
  | prim "EMPTY_MAP" [ty1; ty2] ->
     PUSH (Tmap (convert_type ty1, convert_type ty2), CMap [])
  | prim "NONE" [ty] ->
     PUSH (Toption (convert_type ty), CNone)
                    *)
  | LEFT (ty, None) ->
    prim "LEFT" [convert_type ty] name
  | RIGHT (ty, None) ->
    prim "RIGHT" [convert_type ty] name

  | LEFT (ty, Some c) ->
    prim "LEFT" [convert_type ty] name ~fields:[c; ""]
  | RIGHT (ty, Some c) ->
    prim "RIGHT" [convert_type ty] name ~fields:[""; c]

  | CONS -> prim "CONS" [] name
  | LOOP loop -> prim "LOOP" [convert_code expand loop] name
  | LOOP_LEFT loop -> prim "LOOP_LEFT" [convert_code expand loop] name
  | ITER body -> prim "ITER" [convert_code expand body] name
  | MAP body -> prim "MAP" [convert_code expand body] name
  | CONTRACT (entry, ty) ->
    let fields = match entry with
      | None -> []
      | Some e -> [e] in
    prim "CONTRACT" [convert_type ty] ~fields name
  | UNPACK ty ->
    prim "UNPACK" [convert_type ty] name
  | INT -> prim "INT" [] name
  | ISNAT -> prim "ISNAT" [] name
  | ABS -> prim "ABS" [] name
  | DUP 1 -> prim "DUP" [] name
  | DUP 0 -> assert false
  | DUP n ->
    if expand then
      convert_code expand @@ ii @@
      SEQ [
        ii @@ DIP (n - 1, ii @@ SEQ [{ expr with ins = DUP 1 }]);
        ii @@ DIG (n-1);
      ]
    else
      prim (Printf.sprintf "D%sP" (String.make n 'U')) [] name

  | SELF entry ->
    let fields = match entry with
      | None -> []
      | Some e -> [e] in
    prim "SELF" [] ~fields name
  | STEPS_TO_QUOTA -> prim "STEPS_TO_QUOTA" [] name
  | CREATE_CONTRACT contract ->
    let p, s, c, f = convert_contract_raw expand contract in
    let p = Micheline.map_node (fun l -> l, None) (fun n -> n) p in
    let s = Micheline.map_node (fun l -> l, None) (fun n -> n) s in
    prim "CREATE_CONTRACT" [seq ([p; s; c] @ (match f with None -> [] | Some f -> [f]))] name

  | XOR -> prim "XOR" [] name
  | AND -> prim "AND" [] name
  | NOT -> prim "NOT" [] name
  | NEG -> prim "NEG" [] name
  | LSL -> prim "LSL" [] name
  | LSR -> prim "LSR"  [] name
  | DIP_DROP (ndip, ndrop) ->
    convert_code expand @@ ii @@ DIP (ndip, ii @@ SEQ [ii @@ DROP ndrop])

  | CDAR (0, field) -> convert_code expand { expr with ins = CAR field }
  | CDDR (0, field) -> convert_code expand { expr with ins = CDR field }
  | CDAR (n, field) ->
    if expand then
      convert_code expand @@ ii @@
      SEQ (LiquidMisc.list_init n (fun _ -> ii @@ CDR None) @
           [{ expr with ins = CAR field }])
    else
      let fields = match field with
        | Some f -> [f]
        | None -> [] in
      prim (Printf.sprintf "C%sAR" (String.make n 'D')) [] name ~fields
  | CDDR (n, field) ->
    if expand then
      convert_code expand @@ ii @@
      SEQ (LiquidMisc.list_init n (fun _ -> ii @@ CDR None) @
           [{ expr with ins = CDR field }])
    else
      let fields = match field with
        | Some f -> [f]
        | None -> [] in
      prim (Printf.sprintf "C%sDR" (String.make n 'D')) [] name ~fields
  | SIZE -> prim "SIZE" [] name
  | IMPLICIT_ACCOUNT -> prim "IMPLICIT_ACCOUNT" [] name
  | SET_DELEGATE -> prim "SET_DELEGATE" [] name

  | EXTENSION (minst, tys) -> prim minst (List.map convert_type tys) name

  | BLOCK_LEVEL when expand -> prim "LEVEL" [] name
  | COLLECT_CALL when expand -> prim "COLLCALL" [] name
  | GET_BALANCE when expand -> prim "GETBAL" [] name
  | IS_IMPLICIT when expand-> prim "ISIMP" [] name

  | BLOCK_LEVEL -> prim "BLOCK_LEVEL" [] name
  | COLLECT_CALL -> prim "COLLECT_CALL" [] name
  | GET_BALANCE -> prim "GET_BALANCE" [] name
  | IS_IMPLICIT -> prim "IS_IMPLICIT" [] name
  | EMPTY_BIG_MAP (k, v) ->
    prim "EMPTY_BIG_MAP" [convert_type k; convert_type v] name

and convert_contract_raw expand c =
  let loc = LiquidLoc.noloc in
  let arg_type = convert_type ~loc c.mic_parameter in
  let root_annots = match c.mic_root with
    | None -> []
    | Some r -> convert_annots [Fannot r] in
  let storage_type = convert_type ~loc c.mic_storage in
  let code = convert_code expand c.mic_code in
  let fee_code = match c.mic_fee_code with
    | None -> None
    | Some mic_fee -> Some (convert_code expand mic_fee) in
  let p = Micheline.Prim(loc, "parameter", [arg_type], root_annots) in
  let s = Micheline.Prim(loc, "storage", [storage_type], []) in
  let c = Micheline.Prim((loc, None), "code", [code], []) in
  let f = match fee_code with
    | None -> None
    | Some fee_code ->
      Some (Micheline.Prim((loc, None), "code", [fee_code], ["@fee"])) in
  (p, s, c, f)

let convert_contract ~expand c =
  let p, s, c, f = convert_contract_raw expand c in
  let mp, tp = Micheline.extract_locations p in
  let ms, ts = Micheline.extract_locations s in
  let code_loc_offset = List.length tp + List.length ts + 1 in

  let mc, loc_table = Micheline.extract_locations c in
  let loc_table = List.map (fun (i, l) ->
      i + code_loc_offset, l
    ) loc_table in

  let mf, loc_table = match f with
    | None -> [], loc_table
    | Some f ->
      let mf, fee_loc_table = Micheline.extract_locations f in
      let fee_loc_table = List.map (fun (i, l) ->
          i + code_loc_offset, l
        ) fee_loc_table in
      [mf], loc_table @ fee_loc_table in

  if !LiquidOptions.verbosity > 1 then
    List.iter (fun (i, (l, s)) ->
        match s with
        | None -> Format.eprintf "%d -> %a@." i LiquidLoc.print_loc l
        | Some s -> Format.eprintf "%d -> %a -> %S@." i LiquidLoc.print_loc l s
      ) loc_table;

  [mp; ms; mc] @ mf, loc_table

let print_program comment_of_loc ppf (c, loc_table) =
  let c = List.map
      (Micheline.inject_locations (fun l ->
           (* { Micheline_printer.comment = Some (string_of_int l) } *)
           { Micheline_printer.comment = None }
         )) c in
  List.iter (fun node ->
      Format.fprintf ppf "%a;@."
        Micheline_printer.print_expr_unwrapped node
    ) c

let string_of_expression exp =
  let buf = Buffer.create 10000 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf 199999 ;
  Format.pp_set_max_indent ppf 99999 ;
  Format.pp_set_max_boxes ppf 99999 ;
  let exp = Micheline.inject_locations (fun l ->
           (* { Micheline_printer.comment = Some (string_of_int l) } *)
           { Micheline_printer.comment = None }
    ) exp
  in
  Format.fprintf ppf "%a"
    Micheline_printer.print_expr_unwrapped exp;
  Format.fprintf ppf "@?" ;
  Buffer.contents buf

let string_of_contract c =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Pretty print Michelson contract@.";
  let buf = Buffer.create 10000 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf 199999 ;
  Format.pp_set_max_indent ppf 99999 ;
  Format.pp_set_max_boxes ppf 99999 ;
  print_program (fun _ -> None) ppf (c, []);
  Format.fprintf ppf "@?" ;
  Buffer.contents buf

let linify s =
  let len = String.length s in
  let b = Buffer.create 10000 in
  let prev_whitespace = ref true in
  for i = 0 to len - 1 do
    match s.[i] with
    | ' ' | '\n' ->
      if not !prev_whitespace then begin
        Buffer.add_char b ' ';
        prev_whitespace := true
      end
    | c ->
      Buffer.add_char b c;
      prev_whitespace := false
  done;
  Buffer.contents b


let line_of_contract c =
  linify (string_of_contract c)

let contract_encoding =
  Micheline.canonical_encoding
    ~variant:"michelson_v1"
    Data_encoding.string |> Data_encoding.list

let json_of_contract c =
  Data_encoding.Json.construct contract_encoding c
  |> Data_encoding_ezjsonm.to_string


let contract_of_json j =
  (* let open Error_monad in *)
  Data_encoding_ezjsonm.from_string j
  |> Data_encoding.Json.destruct contract_encoding

let contract_of_ezjson ezj =
  Data_encoding.Json.destruct contract_encoding ezj

let const_encoding =
  Micheline.canonical_encoding
    ~variant:"michelson_v1"
    Data_encoding.string
(* Micheline.erased_encoding 0 Data_encoding.string *)


let string_of_const c = string_of_expression c

let line_of_const c =
  linify (string_of_const c)

let json_of_const c =
  Data_encoding.Json.construct const_encoding c
  |> Data_encoding_ezjsonm.to_string

let const_of_json j =
  Data_encoding_ezjsonm.from_string j
  |> Data_encoding.Json.destruct const_encoding

let const_of_ezjson ezj =
  Data_encoding.Json.destruct const_encoding ezj

(* let read_file = FileString.read_file *)

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  begin try
      while true; do
        lines := input_line chan :: !lines
      done;
    with
      End_of_file -> close_in chan
  end;
  !lines |> List.rev |> String.concat "\n"

let read_micheline_file filename =
  let s = read_file filename in
  match LiquidFromMicheline.contract_of_string filename s with
  | Some (code, loc_table) ->
    Printf.eprintf "Program %S parsed\n%!" filename;
    code, loc_table
  | None ->
    Printf.eprintf "Errors parsing in %S\n%!" filename;
    exit 2

let read_micheline_json filename =
  let s = read_file filename in
  let nodes = contract_of_json s in
  let env = LiquidMichelineTypes.empty_env filename in
  nodes, env


let convert_const ~expand c =
  convert_const ~loc:(LiquidLoc.noloc, None) expand c
  |> Micheline.strip_locations

let convert_type ty =
  convert_type ~loc:(LiquidLoc.noloc, None) ty |> Micheline.strip_locations

let arg_list work_done = [
]

(* force linking not anymore ?
   let execute = Script_interpreter.execute
*)
