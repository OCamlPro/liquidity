(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes
open Micheline

type tezos_code = (unit,string) Micheline.node

let ii i = { i; noloc_name  = None }

let prim name args annot =
  let annot = match annot with
    | Some s -> Some ("@" ^ s)
    | None -> None
  in
  Micheline.Prim(0, name, args, annot)

let prim_type name args = Micheline.Prim(0, name, args, None)

let rec convert_const expr =
  match expr with
  | CInt n -> Micheline.Int (0, LiquidPrinter.mic_of_integer n)
  | CString s -> Micheline.String (0, s)
  | CUnit -> Micheline.Prim(0, "Unit", [], None)
  | CBool true -> Micheline.Prim(0, "True", [], None)
  | CBool false -> Micheline.Prim(0, "False", [], None)
  | CNone -> Micheline.Prim(0, "None", [], None)

  | CSome x -> Micheline.Prim(0, "Some", [convert_const x], None)
  | CLeft x -> Micheline.Prim(0, "Left", [convert_const x], None)
  | CRight x -> Micheline.Prim(0, "Right", [convert_const x], None)

  | CTuple [] -> assert false
  | CTuple [_] -> assert false
  | CTuple [x;y] ->
     Micheline.Prim(0, "Pair", [convert_const x;
                                  convert_const y], None)
  | CTuple (x :: y) ->
     Micheline.Prim(0, "Pair", [convert_const x;
                                  convert_const (CTuple y)], None)
  | CList args -> Micheline.Prim(0, "List",
                                   List.map convert_const args, None)

  | CMap args ->
     Micheline.Prim(0, "Map",
                      List.map (fun (x,y) ->
                          Micheline.Prim(0, "Item", [convert_const x;
                                                       convert_const y], None
                                          ))
                               args, None)
  | CSet args -> Micheline.Prim(0, "Set",
                                  List.map convert_const args, None)
  | CNat n -> Micheline.Int (0, LiquidPrinter.mic_of_integer n)
  | CTez n -> Micheline.String (0, LiquidPrinter.mic_of_tez n)
           (*
  | CTez tez
    |CKey _|
   | CSignature _|CLeft _|CRight _)
            *)
  | CTimestamp s -> Micheline.String (0, s)
  | CKey s -> Micheline.String (0, s)
  | CKey_hash s -> Micheline.String (0, s)
  | CSignature s -> Micheline.String (0, s)

  | _ ->
    LiquidLoc.raise_error "to-tezos: unimplemented const:\n%s%!"
      (LiquidPrinter.Michelson.string_of_const expr)


let rec convert_type expr =
  match expr with
  | Tunit -> prim_type "unit" []
  | Ttimestamp -> prim_type "timestamp" []
  | Ttez -> prim_type "tez" []
  | Tint -> prim_type "int" []
  | Tnat -> prim_type "nat" []
  | Tbool -> prim_type "bool" []
  | Tkey -> prim_type "key" []
  | Tkey_hash -> prim_type "key_hash" []
  | Tsignature -> prim_type "signature" []
  | Tstring -> prim_type "string" []
  | Ttuple [x] -> assert false
  | Ttuple [] -> assert false
  | Ttuple [x;y] -> prim_type "pair" [convert_type x; convert_type y]
  | Ttuple (x :: tys) ->
     prim_type "pair" [convert_type x; convert_type (Ttuple tys)]
  | Tor (x,y) -> prim_type "or" [convert_type x; convert_type y]
  | Tcontract (x,y) -> prim_type "contract" [convert_type x;convert_type y]
  | Tlambda (x,y) -> prim_type "lambda" [convert_type x; convert_type y]
  | Tclosure ((x,e),r) ->
    convert_type (Ttuple [Tlambda (Ttuple [x; e], r); e ]);
  | Tmap (x,y) -> prim_type "map" [convert_type x;convert_type y]
  | Tset x -> prim_type "set" [convert_type x]
  | Tlist x -> prim_type "list" [convert_type x]
  | Toption x -> prim_type "option" [convert_type x]
  | Tfail | Trecord _ | Tsum _ -> assert false

let rec convert_code expr =
  let name = expr.noloc_name in
  match expr.i with
  | ANNOT a ->
    Micheline.Seq (0, [], Some ("@"^a))
  | SEQ exprs ->
    Micheline.Seq (0, List.map convert_code exprs, name)
  | DROP -> prim "DROP" [] name
  | DIP (0, arg) -> assert false
  | DIP (1, arg) -> prim "DIP" [ convert_code arg ] name
  | DIP (n, arg) -> prim (Printf.sprintf "D%sP"
                                         (String.make n 'I'))
                         [ convert_code arg ] name
  | CAR -> prim "CAR" [] name
  | CDR -> prim "CDR" [] name
  | SWAP -> prim "SWAP" [] name
  | IF (x,y) -> prim "IF" [convert_code x; convert_code y] name
  | IF_NONE (x,y) -> prim "IF_NONE" [convert_code x; convert_code y] name
  | IF_LEFT (x,y) -> prim "IF_LEFT" [convert_code x; convert_code y] name
  | IF_CONS (x,y) -> prim "IF_CONS" [convert_code x; convert_code y] name
  | NOW -> prim "NOW" [] name
  | PAIR -> prim "PAIR" [] name
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
  | MANAGER -> prim "MANAGER" [] name
  | SOURCE (ty1,ty2) ->
     prim "SOURCE" [convert_type ty1; convert_type ty2] name
  | MAP -> prim "MAP" [] name
  | OR -> prim "OR" [] name
  | LAMBDA (ty1, ty2, expr) ->
     prim "LAMBDA" [convert_type ty1; convert_type ty2; convert_code expr] name
  | REDUCE -> prim "REDUCE" [] name
  | COMPARE -> prim "COMPARE" [] name
  | FAIL -> prim "FAIL" [] name
  | PUSH (Tunit, CUnit) -> prim "UNIT" [] name
  | TRANSFER_TOKENS -> prim "TRANSFER_TOKENS" [] name
  | PUSH (ty, cst) -> prim "PUSH" [ convert_type ty;
                                    convert_const cst ] name
  | H -> prim "H" [] name
  | HASH_KEY -> prim "HASH_KEY" [] name
  | CHECK_SIGNATURE -> prim "CHECK_SIGNATURE" [] name
  | CONCAT -> prim "CONCAT" [] name
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
  | LEFT ty ->
     prim "LEFT" [convert_type ty] name
  | CONS -> prim "CONS" [] name
  | LOOP loop -> prim "LOOP" [convert_code loop] name
  | ITER body -> prim "ITER" [convert_code body] name
  | RIGHT ty ->
     prim "RIGHT" [convert_type ty] name
  | INT -> prim "INT" [] name
  | ABS -> prim "ABS" [] name
  | DUP 1 -> prim "DUP" [] name
  | DUP 0 -> assert false
  | DUP n ->
    prim (Printf.sprintf "D%sP" (String.make n 'U')) [] name

  | SELF -> prim "SELF" [] name
  | STEPS_TO_QUOTA -> prim "STEPS_TO_QUOTA" [] name
  | CREATE_ACCOUNT -> prim "CREATE_ACCOUNT" [] name
  | CREATE_CONTRACT -> prim "CREATE_CONTRACT" [] name

  | XOR -> prim "XOR" [] name
  | AND -> prim "AND" [] name
  | NOT -> prim "NOT" [] name
  | NEG -> prim "NEG" [] name
  | LSL -> prim "LSL" [] name
  | LSR -> prim "LSR"  [] name
  | DIP_DROP (ndip, ndrop) ->
    convert_code @@
    ii @@ DIP (ndip, ii @@ SEQ (LiquidMisc.list_init ndrop (fun _ -> ii DROP)))

  | CDAR n -> prim (Printf.sprintf "C%sAR" (String.make n 'D')) [] name
  | CDDR n -> prim (Printf.sprintf "C%sDR" (String.make n 'D')) [] name
  | SIZE -> prim "SIZE" [] name
  | DEFAULT_ACCOUNT -> prim "DEFAULT_ACCOUNT" [] name


let convert_contract c =
  let ret_type = convert_type c.return in
  let arg_type = convert_type c.parameter in
  let storage_type = convert_type c.storage in
  let code = convert_code c.code in
  let nodes = Micheline.Prim(0, "return", [ret_type], None) ::
                Micheline.Prim(0, "parameter", [arg_type], None) ::
                  Micheline.Prim(0, "storage", [storage_type], None) ::
                    Micheline.Prim(0, "code", [code], None) ::
                      []
  in
  List.map Micheline.strip_locations nodes

let print_program comment_of_loc ppf (c, loc_table) =
  let c = List.map (Micheline.inject_locations
                          (fun _ -> { Micheline_printer.comment = None })
                       ) c in
  List.iter (fun node ->
      Format.fprintf  ppf
                      "%a;@."
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
  Format.pp_set_max_boxes ppf 0;
  Format.pp_set_max_indent ppf 0;
  print_program (fun _ -> None) ppf (c, []);
  let s = Format.flush_str_formatter () in
  Format.pp_set_formatter_out_functions ppf ffs;
  s

let read_tezos_file filename =
  let s = FileString.read_file filename in
  let contract_hash = Hash.Operation_hash.hash_bytes [s] in
  match LiquidFromTezos.contract_of_string filename s with
  | Some (code, loc_table) ->
     Printf.eprintf "Program %S parsed\n%!" filename;
     code, contract_hash, loc_table
  | None ->
     Printf.eprintf "Errors parsing in %S\n%!" filename;
     exit 2



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
