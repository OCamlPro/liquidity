(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

type init =
  | Init_constant of LiquidTypes.const
  | Init_code of LiquidTypes.noloc_michelson_contract

let c_unit = mk (Const (Tunit, CUnit)) ()
let mk_nat i = mk (Const (Tnat, CNat (LiquidPrinter.integer_of_int i))) ()

let tmp_contract_of_init (args, code) storage_ty =
  let return = storage_ty in
  let storage = Tunit in
  let parameter = Ttuple (List.map (fun (_,_,ty) -> ty) args) in
  let parameter_var = mk (Var ("parameter", LiquidLoc.noloc, [])) () in
  let code, _ = List.fold_right (fun (arg, loc, ty) (code, i) ->
      let i = i - 1 in
      let code = mk (
          Let (arg, loc,
               mk (Apply (Prim_tuple_get, loc, [parameter_var; mk_nat i])) (),
               code)) ()
      in
      (code, i)
    ) args (code, List.length args)
  in
  let code = mk(Apply (Prim_tuple, LiquidLoc.noloc, [code; c_unit])) () in
  { parameter; storage; return; code }

let compile_liquid_init env contract ((args, sy_init) as init) =
  if sy_init.transfer then
    LiquidLoc.raise_error ~loc:(LiquidLoc.loc_in_file env.filename)
      "No transfer allowed in storage initializer";
  try (* Maybe it is constant *)
    let ty_init = LiquidCheck.typecheck_code
        ~warnings:true env contract contract.storage sy_init in
    let enc_init = LiquidEncode.encode_code env contract ty_init in
    let c_init = LiquidData.translate_const_exp LiquidLoc.noloc enc_init in
    Init_constant c_init
  (* let s = LiquidPrinter.Michelson.line_of_const c_init in
   * let output = env.filename ^ ".init.tz" in
   * FileString.write_file output s;
   * Printf.eprintf "Constant initial storage generated in %S\n%!" output *)
  with LiquidError _ ->
    (* non constant initial value *)
    let init_contract = tmp_contract_of_init init contract.storage in
    let typed_init = LiquidCheck.typecheck_contract
        ~warnings:true env init_contract in
    let encoded_init, _ = LiquidEncode.encode_contract env typed_init in
    let pre_init = LiquidMichelson.translate encoded_init in
    Init_code pre_init
(* let mic_init = LiquidToTezos.convert_contract pre_init in
 * let s = LiquidToTezos.line_of_contract mic_init in
 * let output = env.filename ^ ".initializer.tz" in
 * FileString.write_file output s;
 * Printf.eprintf "Storage initializer generated in %S\n%!" output *)
