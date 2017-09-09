(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(*
type expr =
  | Int of location * string
  | String of location * string
  | Prim of location * string * expr list
  | Seq of location * expr list
 *)

let prim name args = Script_repr.Prim(0, name, args)

let rec convert_const expr =
  match expr with
  | CInt n -> Script_repr.Int (0, n)
  | CString s -> Script_repr.String (0, s)
  | CUnit -> Script_repr.Prim(0, "Unit", [])
  | CBool true -> Script_repr.Prim(0, "True", [])
  | CBool false -> Script_repr.Prim(0, "False", [])
  | CNone -> Script_repr.Prim(0, "None", [])

  | CSome x -> Script_repr.Prim(0, "Some", [convert_const x])
  | CTuple [] -> assert false
  | CTuple [_] -> assert false
  | CTuple [x;y] ->
     Script_repr.Prim(0, "Pair", [convert_const x;
                                  convert_const y])
  | CTuple (x :: y) ->
     Script_repr.Prim(0, "Pair", [convert_const x;
                                  convert_const (CTuple y)])
  | CList args -> Script_repr.Prim(0, "List",
                                   List.map convert_const args)
  | CMap args ->
     Script_repr.Prim(0, "Map",
                      List.map (fun (x,y) ->
                          Script_repr.Prim(0, "Item", [convert_const x;
                                                       convert_const y]))
                               args)
  | CSet args -> Script_repr.Prim(0, "Set",
                                  List.map convert_const args)
  | CNat n -> Script_repr.Int (0, n)
  | CTez n -> Script_repr.String (0, n)
           (*
  | CTez tez
    |CKey _|
   | CSignature _|CLeft _|CRight _)
            *)
  | _ ->
     Printf.eprintf "to-tezos: unimplemented const:\n%s\n%!"
                    (LiquidPrinter.Michelson.string_of_const expr);
     raise Error


let rec convert_type expr =
  match expr with
  | Tunit -> prim "unit" []
  | Ttimestamp -> prim "timestamp" []
  | Ttez -> prim "tez" []
  | Tint -> prim "int" []
  | Tnat -> prim "nat" []
  | Tbool -> prim "bool" []
  | Tkey -> prim "key" []
  | Tsignature -> prim "signature" []
  | Tstring -> prim "string" []
  | Ttuple [x] -> assert false
  | Ttuple [] -> assert false
  | Ttuple [x;y] -> prim "pair" [convert_type x; convert_type y]
  | Ttuple (x :: tys) ->
     prim "pair" [convert_type x; convert_type (Ttuple tys)]
  | Tor (x,y) -> prim "or" [convert_type x; convert_type y]
  | Tcontract (x,y) -> prim "contract" [convert_type x;convert_type y]
  | Tlambda (x,y) -> prim "lambda" [convert_type x; convert_type y]
  | Tmap (x,y) -> prim "map" [convert_type x;convert_type y]
  | Tset x -> prim "set" [convert_type x]
  | Tlist x -> prim "list" [convert_type x]
  | Toption x -> prim "option" [convert_type x]
  | Tfail -> assert false
  | Ttype (_, ty) -> convert_type ty

let rec convert_code expr =
  match expr with
  | SEQ exprs ->
     Script_repr.Seq (0, List.map convert_code exprs)
  | DROP -> prim "DROP" []
  | DIP (0, arg) -> assert false
  | DIP (1, arg) -> prim "DIP" [ convert_code arg ]
  | DIP (n, arg) -> prim (Printf.sprintf "D%sP"
                                         (String.make n 'I'))
                         [ convert_code arg ]
  | CAR -> prim "CAR" []
  | CDR -> prim "CDR" []
  | SWAP -> prim "SWAP" []
  | IF (x,y) -> prim "IF" [convert_code x; convert_code y]
  | IF_NONE (x,y) -> prim "IF_NONE" [convert_code x; convert_code y]
  | IF_LEFT (x,y) -> prim "IF_LEFT" [convert_code x; convert_code y]
  | IF_CONS (x,y) -> prim "IF_CONS" [convert_code x; convert_code y]
  | NOW -> prim "NOW" []
  | PAIR -> prim "PAIR" []
  | BALANCE -> prim "BALANCE" []
  | SUB -> prim "SUB" []
  | ADD -> prim "ADD" []
  | MUL -> prim "MUL" []
  | NEQ -> prim "NEQ" []
  | EQ -> prim "EQ" []
  | LT -> prim "LT" []
  | LE -> prim "LE" []
  | GT -> prim "GT" []
  | GE -> prim "GE" []
  | GET -> prim "GET" []
  | UPDATE -> prim "UPDATE" []
  | MEM -> prim "MEM" []
  | SOME -> prim "SOME" []
  | MANAGER -> prim "MANAGER" []
  | SOURCE (ty1,ty2) ->
     prim "SOURCE" [convert_type ty1; convert_type ty2]
  | MAP -> prim "MAP" []
  | OR -> prim "OR" []
  | LAMBDA (ty1, ty2, expr) ->
     prim "LAMBDA" [convert_type ty1; convert_type ty2; convert_code expr]
  | REDUCE -> prim "REDUCE" []
  | COMPARE -> prim "COMPARE" []
  | FAIL -> prim "FAIL" []
  | PUSH (Tunit, CUnit) -> prim "UNIT" []
  | TRANSFER_TOKENS -> prim "TRANSFER_TOKENS" []
  | PUSH (ty, cst) -> prim "PUSH" [ convert_type ty;
                                    convert_const cst ]
  | H -> prim "H" []
  | CHECK_SIGNATURE -> prim "CHECK_SIGNATURE" []
  | CONCAT -> prim "CONCAT" []
  | EDIV -> prim "EDIV" []
  | EXEC -> prim "EXEC" []
  | MOD -> prim "MOD" []
  | DIV -> prim "DIV" []
  | AMOUNT -> prim "AMOUNT" []
                   (*
  | prim "EMPTY_MAP" [ty1; ty2] ->
     PUSH (Tmap (convert_type ty1, convert_type ty2), CMap [])
  | prim "NONE" [ty] ->
     PUSH (Toption (convert_type ty), CNone)
                    *)
  | LEFT ty ->
     prim "LEFT" [convert_type ty]
  | CONS -> prim "CONS" []
  | LOOP loop -> prim "LOOP" [convert_code loop]
  | RIGHT ty ->
     prim "RIGHT" [convert_type ty]
  | INT -> prim "INT" []
  | ABS -> prim "ABS" []
  | DUP 1 -> prim "DUP" []
  | DUP 0 -> assert false
  | DUP n ->
     prim (Printf.sprintf "D%sP" (String.make n 'U')) []


  | SELF -> prim "SELF" []
  | STEPS_TO_QUOTA -> prim "STEPS_TO_QUOTA" []
  | CREATE_ACCOUNT -> prim "CREATE_ACCOUNT" []
  | CREATE_CONTRACT -> prim "CREATE_CONTRACT" []

  | XOR -> prim "XOR" []
  | AND -> prim "AND" []
  | NOT -> prim "NOT" []
  | NEG -> prim "NEG" []
  | LSL -> prim "LSL" []
  | LSR -> prim "LSR"  []
  | DIP_DROP (ndip, ndrop) ->
     convert_code (DIP (ndip, SEQ (LiquidMisc.list_init ndrop (fun _ -> DROP))))
  | CDAR n -> prim (Printf.sprintf "C%sAR" (String.make n 'D')) []
  | CDDR n -> prim (Printf.sprintf "C%sDR" (String.make n 'D')) []


let convert_contract c =
  let ret_type = convert_type c.return in
  let arg_type = convert_type c.parameter in
  let storage_type = convert_type c.storage in
  let code = convert_code c.code in
  {
    Script_repr.ret_type;
    Script_repr.arg_type;
    Script_repr.storage_type;
    Script_repr.code;
  }

let string_of_contract c =
  let ppf = Format.str_formatter in
  Client_proto_programs.print_program (fun _ -> None) ppf (c, []);
  Format.flush_str_formatter ()
