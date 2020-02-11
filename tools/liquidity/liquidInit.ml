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

type 'a init =
  | Init_constant of (datatype, 'a) exp const
  | Init_code of ((datatype, 'a) exp contract)

let c_empty_op ~loc =
  mk ~loc (Const { ty = Tlist Toperation; const = CList []}) (Tlist Toperation)
let mk_nat ~loc i =
  mk ~loc
    (Const { ty = Tnat; const = CNat (LiquidNumber.integer_of_int i) })
    Tnat


let tmp_contract_of_init ~loc env (init : (datatype, 'a) exp LiquidTypes.init) storage_ty =
  (* let init =
   *   { init with init_body = (LiquidUntype.untype_code init.init_body : syntax_exp) } in *)
  let storage = storage_ty in
  let parameter, code = match init.init_args with
    | [] -> Tunit, init.init_body
    | [arg, loc, ty] ->
      let parameter_var = mk ~loc (Var "_parameter") ty in
      let code = mk ~loc
          (Let { bnd_var = { nname = arg; nloc = loc };
                 inline = InAuto;
                 bnd_val = parameter_var;
                 body = init.init_body }) init.init_body.ty in
      ty, code
    | args ->
      let parameter = Ttuple (List.map (fun (_,_,ty) -> ty) args) in
      let parameter_var = mk ~loc (Var "_parameter") parameter in
      let code, _ = List.fold_right (fun (arg, loc, ty) (code, i) ->
          let i = i - 1 in
          let code = mk ~loc (
              Let { bnd_var = { nname = arg; nloc = loc };
                    inline = InAuto;
                    bnd_val = mk ~loc (Apply {
                        prim = Prim_tuple_get;
                        args = [parameter_var; mk_nat ~loc i] })
                        ty;
                    body = code }) code.ty
          in
          (code, i)
        ) args (init.init_body, List.length args)
      in
      parameter, code
  in
  let code =
    mk ~loc (Apply { prim = Prim_tuple;
                     args = [ c_empty_op ~loc; code ] })
      (Ttuple [(Tlist Toperation); code.ty]) in
  { contract_name = "_dummy_init";
    storage;
    values = [];
    entries = [{ entry_sig = { entry_name = "default";
                               parameter;
                               parameter_name = "_parameter";
                               storage_name = "_storage" };
                 code; fee_code = None }];
    ty_env = env;
    c_init = None;
    subs = [];
  }

let compile_liquid_init env contract_sig (init : (datatype, 'a) exp LiquidTypes.init) =
  let loc = init.init_body.loc in
  if init.init_body.transfer then
    LiquidLoc.raise_error ~loc
      "No transfer allowed in storage initializer";
  try (* Maybe it is constant *)
    let c_init = LiquidData.translate_const_exp init.init_body in
    Init_constant c_init
  with LiquidError _ ->
    (* non constant initial value *)
    let init_contract = tmp_contract_of_init ~loc env init contract_sig.f_storage in
    Init_code init_contract
