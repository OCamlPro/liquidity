(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO: We could use a simplify pass to propagate the no-effect
  defining expression of once-used variables to their uniq use
  site. It could dramatically decrease the size of the stack.  *)


open LiquidTypes

let rec translate_const_exp loc exp =
  match exp.desc with
  | Let (_, loc, _, _) ->
     LiquidLoc.raise_error ~loc "'let' forbidden in constant"
  | Const (ty, c) -> c

  (* removed during typechecking *)
  | Record (_, _)
    | Constructor (_, _, _) -> assert false

  | Apply (Prim_Left, _, [x; _ty]) -> CLeft (translate_const_exp loc x)
  | Apply (Prim_Right, _, [x; _ty]) -> CRight (translate_const_exp loc x)
  | Apply (Prim_Some, _, [x]) -> CSome (translate_const_exp loc x)
  | Apply (Prim_Cons, _, list) ->
     CList (List.map (translate_const_exp loc) list)
  | Apply (Prim_tuple, _, list) ->
     CTuple (List.map (translate_const_exp loc) list)


  | Apply (prim, _, args)
    -> LiquidLoc.raise_error "<apply %s(%d) not yet implemented>"
                             (LiquidTypes.string_of_primitive prim)
                             (List.length args)
  | Var (_, _, _)
    | SetVar (_, _, _, _)
    | If (_, _, _)
    | Seq (_, _)
    | LetTransfer (_, _, _, _, _, _, _, _)
    | MatchOption (_, _, _, _, _)
    | MatchNat (_, _, _, _, _, _)
    | MatchList (_, _, _, _, _, _)
    | Loop (_, _, _, _)
    | Lambda (_, _, _, _, _)
    | Closure (_, _, _, _, _, _)
    | MatchVariant (_, _, _)
    ->
     LiquidLoc.raise_error ~loc "non-constant expression"

let data_of_liq ~filename ~contract ~parameter ~storage =
  (* first, extract the types *)
  let ocaml_ast = LiquidFromOCaml.structure_of_string
                    ~filename contract in
  let contract, env = LiquidFromOCaml.translate ~filename ocaml_ast in
  let _ = LiquidCheck.typecheck_contract
               ~warnings:true env contract in

  let translate filename s ty =
    try
      let ml_exp = LiquidFromOCaml.expression_of_string ~filename s in
      let sy_exp = LiquidFromOCaml.translate_expression env ml_exp in
      let ty_exp = LiquidCheck.typecheck_code
          ~warnings:true env contract ty sy_exp in
      let loc = LiquidLoc.loc_in_file filename in
      let ty_exp = translate_const_exp loc ty_exp in
      let s = LiquidPrinter.Michelson.line_of_const ty_exp in
      Ok s
    with LiquidError error ->
      Error error
  in
  (translate "parameter" parameter contract.parameter),
  (translate "storage" storage contract.storage)
