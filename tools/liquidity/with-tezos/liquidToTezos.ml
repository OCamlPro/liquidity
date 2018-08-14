(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes
open Michelson_Tezos
open Micheline

type loc_table = (int * (LiquidTypes.location * string option)) list

type tezos_code = (unit,string) Micheline.node

let ii ~loc ins = { ins; loc; loc_name  = None }

let loc_of_many (l : loc_michelson list) = match l, List.rev l with
  | [], _ | _, [] -> LiquidLoc.noloc
  | first :: _, last :: _ -> LiquidLoc.merge first.loc last.loc

let prim ~loc ?(fields=[]) name args var_name =
  let annots = List.map (fun f -> "%" ^ f) fields in
  let annots = match var_name with
    | Some s -> ("@" ^ s) :: annots
    | None -> annots
  in
  Micheline.Prim(loc, name, args, annots)

let seq ~loc exprs =
  Micheline.Seq(loc, exprs)

let prim_type ~loc ?(annots=[]) name args =
  Micheline.Prim(loc, name, args, annots)

let rec convert_const ~loc expr =
  let bytes_of_hex s =
    `Hex (String.sub s 2 (String.length s - 2))
    |> MBytes.of_hex in
  match expr with
  | CInt n -> Micheline.Int (loc, LiquidPrinter.mic_of_integer n)
  | CString s -> Micheline.String (loc, s)
  | CBytes s -> Micheline.Bytes (loc, bytes_of_hex s)
  | CUnit -> Micheline.Prim(loc, "Unit", [], [])
  | CBool true -> Micheline.Prim(loc, "True", [], [])
  | CBool false -> Micheline.Prim(loc, "False", [], [])
  | CNone -> Micheline.Prim(loc, "None", [], [])

  | CSome x -> Micheline.Prim(loc, "Some", [convert_const ~loc x], [])
  | CLeft x -> Micheline.Prim(loc, "Left", [convert_const ~loc x], [])
  | CRight x -> Micheline.Prim(loc, "Right", [convert_const ~loc x], [])

  | CTuple [] -> assert false
  | CTuple [_] -> assert false
  | CTuple [x;y] ->
     Micheline.Prim(loc, "Pair", [convert_const ~loc x;
                                  convert_const ~loc y], [])
  | CTuple (x :: y) ->
     Micheline.Prim(loc, "Pair", [convert_const ~loc x;
                                  convert_const ~loc (CTuple y)], [])

  | CList args | CSet args ->
    Micheline.Seq(loc, List.map (convert_const ~loc) args)

  | CMap args | CBigMap args ->
     Micheline.Seq(loc,
                      List.map (fun (x,y) ->
                          Micheline.Prim(loc, "Elt", [convert_const ~loc x;
                                                       convert_const ~loc y], []
                                          ))
                               args)

  | CNat n -> Micheline.Int (loc, LiquidPrinter.mic_of_integer n)
  | CTez n -> Micheline.Int (loc, LiquidPrinter.mic_mutez_of_tez n)
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
  | CContract s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CContract s -> Micheline.String (loc, s)
  | CAddress s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CAddress s -> Micheline.String (loc, s)
  | CSignature s when s.[0] = '0' -> Micheline.Bytes (loc, bytes_of_hex s)
  | CSignature s -> Micheline.String (loc, s)

  | _ ->
    LiquidLoc.raise_error ~loc:(fst loc) "to-tezos: unimplemented const:\n%s%!"
      (LiquidPrinter.Michelson.string_of_const expr)


let rec convert_type ~loc expr =
  match expr with
  | Tunit -> prim_type ~loc "unit" []
  | Ttimestamp -> prim_type ~loc "timestamp" []
  | Ttez -> prim_type ~loc "mutez" []
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
  | Ttuple [x] -> assert false
  | Ttuple [] -> assert false
  | Ttuple [x;y] ->
    prim_type ~loc "pair" [convert_type ~loc x; convert_type ~loc y]
  | Ttuple (x :: tys) ->
     prim_type ~loc "pair" [convert_type ~loc x; convert_type ~loc (Ttuple tys)]
  | Tor (x,y) -> prim_type ~loc "or" [convert_type ~loc x; convert_type ~loc y]
  | Tcontract { sig_name; entries_sig = [{ parameter }]} ->
    let annots = match sig_name with
      | None -> []
      | Some n -> [":" ^ n] in
    prim_type ~loc "contract" [convert_type ~loc parameter] ~annots
  | Tcontract _ -> assert false
  | Tlambda (x,y) -> prim_type ~loc "lambda" [convert_type ~loc x;
                                         convert_type ~loc y]
  | Tclosure ((x,e),r) ->
    convert_type ~loc (Ttuple [Tlambda (Ttuple [x; e], r); e ]);
  | Tmap (x,y) -> prim_type ~loc "map" [convert_type ~loc x;convert_type ~loc y]
  | Tbigmap (x,y) ->
    prim_type ~loc "big_map" [convert_type ~loc x;convert_type ~loc y]
  | Tset x -> prim_type ~loc "set" [convert_type ~loc x]
  | Tlist x -> prim_type ~loc "list" [convert_type ~loc x]
  | Toption x -> prim_type ~loc "option" [convert_type ~loc x]
  | Trecord (name, labels) -> convert_record_type ~loc name labels
  | Tsum (name, constrs) -> convert_sum_type ~loc name constrs
  | Tfail -> assert false

and convert_record_type ~loc name labels =
  convert_composed_type "pair" ~loc name labels

and convert_sum_type ~loc name constrs =
  convert_composed_type "or" ~loc name constrs

and convert_composed_type ty_c ~loc name labels =
  match labels with
  | [] -> assert false
  | [l, ty] ->
    begin match convert_type ~loc ty with
      | Micheline.Prim(loc, "big_map", args, annots) ->
        Micheline.Prim(loc, "big_map", args, [":"^l])
      | Micheline.Prim(loc, name, args, annots) ->
        Micheline.Prim(loc, name, args, annots @ ["%"^l])
      | _ -> assert false
    end
  | (l, ty) :: labels ->
    let annots = if name = "" then [] else [":"^name] in
    let ty = match convert_type ~loc ty with
      | Micheline.Prim(loc, "big_map", args, annots) ->
        Micheline.Prim(loc, "big_map", args, [":"^l])
      | Micheline.Prim(loc, name, args, annots) ->
        Micheline.Prim(loc, name, args, annots @ ["%"^l])
      | _ -> assert false in
    prim_type ~loc ~annots ty_c
      [ty; convert_composed_type ty_c ~loc "" labels]

let rec convert_code expand expr =
  let name = expr.loc_name in
  let ii = ii ~loc:expr.loc in
  let seq = seq ~loc:(expr.loc, None) in
  let prim = prim ~loc:(expr.loc, None) in
  let convert_type = convert_type ~loc:(expr.loc, None) in
  let convert_const = convert_const ~loc:(expr.loc, None) in
  match expr.ins with
  | RENAME a -> prim "RENAME" [] a
  | SEQ exprs -> seq (List.map (convert_code expand) exprs)

  | FAILWITH -> prim "FAILWITH" [] name

  | DROP -> prim "DROP" [] name
  | DIP (0, arg) -> assert false
  | DIP (1, arg) -> prim "DIP" [ convert_code expand arg ] name
  | DIP (n, arg) ->
    if expand then
      prim "DIP" [ convert_code expand @@ ii @@
                   SEQ [{ expr with ins = DIP(n-1, arg)}]
                 ] None
    else
      prim (Printf.sprintf "D%sP" (String.make n 'I'))
        [ convert_code expand arg ] name
  | CAR None -> prim "CAR" []  name
  | CAR (Some field) -> prim "CAR" [] ~fields:[field] name
  | CDR None -> prim "CDR" []  name
  | CDR (Some field) -> prim "CDR" [] ~fields:[field] name
  | SWAP -> prim "SWAP" [] name
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
  | ITER body -> prim "ITER" [convert_code expand body] name
  | MAP body -> prim "MAP" [convert_code expand body] name
  | CONTRACT ty ->
     prim "CONTRACT" [convert_type ty] name
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
        ii @@ DIP(1, ii @@ SEQ [{ expr with ins = DUP(n-1) }]);
        ii SWAP
      ]
    else
      prim (Printf.sprintf "D%sP" (String.make n 'U')) [] name

  | SELF -> prim "SELF" [] name
  | STEPS_TO_QUOTA -> prim "STEPS_TO_QUOTA" [] name
  | CREATE_ACCOUNT -> prim "CREATE_ACCOUNT" [] name
  | CREATE_CONTRACT contract ->
    let p, s, c = convert_contract_raw expand contract in
    let p = Micheline.map_node (fun l -> l, None) (fun n -> n) p in
    let s = Micheline.map_node (fun l -> l, None) (fun n -> n) s in
    prim "CREATE_CONTRACT" [seq [p; s; c]] name

  | XOR -> prim "XOR" [] name
  | AND -> prim "AND" [] name
  | NOT -> prim "NOT" [] name
  | NEG -> prim "NEG" [] name
  | LSL -> prim "LSL" [] name
  | LSR -> prim "LSR"  [] name
  | DIP_DROP (ndip, ndrop) ->
    convert_code expand @@
    ii @@ DIP (ndip, ii @@ SEQ (LiquidMisc.list_init ndrop (fun _ -> ii DROP)))

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

and convert_contract_raw expand c =
  let loc = LiquidLoc.noloc in
  let arg_type = convert_type ~loc c.mic_parameter in
  let storage_type = convert_type ~loc c.mic_storage in
  let code = convert_code expand c.mic_code in
  let p = Micheline.Prim(loc, "parameter", [arg_type], []) in
  let s = Micheline.Prim(loc, "storage", [storage_type], []) in
  let c = Micheline.Prim((loc, None), "code", [code], []) in
  (p, s, c)

let convert_contract ~expand c =
  let p, s, c = convert_contract_raw expand c in
  let mp, tp = Micheline.extract_locations p in
  let ms, ts = Micheline.extract_locations s in
  let code_loc_offset = List.length tp + List.length ts + 1 in

  let mc, loc_table = Micheline.extract_locations c in
  let loc_table = List.map (fun (i, l) ->
      i + code_loc_offset, l
    ) loc_table in

  if !LiquidOptions.verbosity > 1 then
    List.iter (fun (i, (l, s)) ->
        match s with
        | None -> Format.eprintf "%d -> %a@." i LiquidLoc.print_loc l
        | Some s -> Format.eprintf "%d -> %a -> %S@." i LiquidLoc.print_loc l s
      ) loc_table;

  [mp; ms; mc], loc_table

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


let string_of_contract c =
  let ppf = Format.str_formatter in
  print_program (fun _ -> None) ppf (c, []);
  Format.flush_str_formatter ()

let line_of_contract c =
  let ppf = Format.str_formatter in
  let ffs = Format.pp_get_formatter_out_functions ppf () in
  let new_ffs =
    { ffs with
      Format.out_newline = (fun () -> ffs.Format.out_spaces 1);
      (* Format.out_indent = (fun _ -> ()); *)
    } in
  Format.pp_set_formatter_out_functions ppf new_ffs;
  print_program (fun _ -> None) ppf (c, []);
  let s = Format.flush_str_formatter () in
  Format.pp_set_formatter_out_functions ppf ffs;
  s

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

let read_tezos_file filename =
  let s = read_file filename in
  match LiquidFromTezos.contract_of_string filename s with
  | Some (code, loc_table) ->
     Printf.eprintf "Program %S parsed\n%!" filename;
     code, loc_table
  | None ->
     Printf.eprintf "Errors parsing in %S\n%!" filename;
     exit 2

let read_tezos_json filename =
  let s = read_file filename in
  let nodes = contract_of_json s in
  let env = LiquidTezosTypes.{ filename;
                               loc_table = IntMap.empty;
                               type_annots = Hashtbl.create 17;
                               types = [];
                               annoted = false;
                             } in
  nodes, env


let convert_const c =
  convert_const ~loc:(LiquidLoc.noloc, None) c |> Micheline.strip_locations

    (*

let contract_amount = ref "1000.00"
let contract_arg = ref (Micheline.Prim(0, "Unit", [], debug))
let contract_storage = ref (Micheline.Prim(0, "Unit", [], debug))

let context = ref None

let get_context () =
  match !context with
  | Some ctxt -> ctxt
  | None ->
     let (level : int32) = 1l in
     let (timestamp : int64) = 1L in
     let (fitness : MBytes.t list) = [] in
     let (ctxt : Context.t) = Context.empty in
     match
       Storage.prepare ~level ~timestamp ~fitness ctxt
     with
     | Error _ -> assert false
     | Ok (ctxt, _bool) ->
        context := Some ctxt;
        ctxt

let execute_contract_file filename =
  assert false
         (*
  let contract, contract_hash, _ = read_tezos_file filename in

  let origination = Contract.initial_origination_nonce contract_hash in
  let destination = Contract.originated_contract origination in

  (* TODO: change that. Why do we need a Source opcode in Michelson ? *)
  let source = destination in

  let ctxt = get_context () in

  let (amount : Tez.t) =
    match Tez_repr.of_string !contract_amount with
    | None -> assert false
    | Some amount -> amount in
  let (storage : Micheline.storage) = {
      Micheline.storage_type = contract.Script.storage_type;
      Micheline.storage = !contract_storage;
    } in
  let (arg : Micheline.expr) = !contract_arg in
  let (qta : int) = 1000 in

  match
    Script_interpreter.execute origination source destination ctxt
                               storage contract amount
                               arg qta
  with
  | Ok (new_storage, result, qta, ctxt, origination) ->
     let ppf = Format.str_formatter in
     let noloc = fun _ -> None in
     Format.fprintf ppf "Result:\n";
     Client_proto_programs.print_expr noloc ppf result;
     Format.fprintf ppf "@.";
     Format.fprintf ppf "Storage:\n";
     Client_proto_programs.print_expr noloc ppf new_storage;
     Format.fprintf ppf "@.";
     let s = Format.flush_str_formatter () in
     Printf.printf "%s\n%!" s;
     contract_storage := new_storage

  | Error errors ->
     Printf.eprintf "%d Errors executing %S\n%!"
                    (List.length errors) filename;
     List.iter (fun error ->
         Format.eprintf "%a" Tezos_context.pp error
       ) errors;
     Tezos_context.pp_print_error Format.err_formatter errors;
     Format.fprintf Format.err_formatter "@.";

     exit 2
          *)
          *)

let arg_list work_done = [
    (*
    "--exec", Arg.String (fun s ->
                  work_done := true;
                  execute_contract_file s),
    "FILE.tz Execute Tezos file FILE.tz";
    "--load-arg", Arg.String (fun s ->
                      let content = FileString.read_file s in
                      match LiquidFromTezos.data_of_string content with
                      | None -> assert false
                      | Some data -> contract_arg := data),
    "FILE Use data from file as argument";
    "--load-storage", Arg.String (fun s ->
                          let content = FileString.read_file s in
                          match LiquidFromTezos.data_of_string content with
                          | None -> assert false
                          | Some data -> contract_storage := data),
    "FILE Use data from file as initial storage";
    "--amount", Arg.String (fun s -> contract_amount := s),
    "NNN.00 Number of Tez sent";
     *)
  ]

(* force linking not anymore ?
let execute = Script_interpreter.execute
 *)
