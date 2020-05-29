(**************************************************************************)
(*                             Dune Network                               *)
(*                                                                        *)
(*  Copyright 2019 Origin-Labs                                            *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  any later version.                                                    *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(**************************************************************************)

open Alpha_context
open Error_monad
open Love_pervasives
open Exceptions
open Love_type
open Love_primitive
open Love_value
open Value

let register = Love_prim_interp.register_primitive

let gas_1 = Gas.step_cost 1
let bad_arguments p args =
  raise (InvariantBroken (Format.asprintf
"Love_primitive %S called with the wrong number or kind of arguments (%d) : %a"
    p.prim_name (List.length args)
    (Utils.print_list_s " ; " Love_printer.Value.print) args
  ))

let tunit = Love_type_list.get_type "unit" []
let tint = Love_type_list.get_type "int" []
let tdun = Love_type_list.get_type "dun" []
let tnat = Love_type_list.get_type "nat" []
let toption t = Love_type_list.get_type "option" [t]
let tlist t = Love_type_list.get_type "list" [t]
let tentrypoint t = Love_type_list.get_type "entrypoint" [t]
let tview (t, t') = Love_type_list.get_type "view" [t; t']
let tset t = Love_type_list.get_type "set" [t]
let tmap (t,t') = Love_type_list.get_type "map" [t; t']
let tbigmap (t,t') = Love_type_list.get_type "bigmap" [t; t']
let tbool = Love_type_list.get_type "bool" []
let taddress = Love_type_list.get_type "address" []
let tkey = Love_type_list.get_type "key" []
let tkeyhash = Love_type_list.get_type "keyhash" []
let ttimestamp = Love_type_list.get_type "timestamp" []
let tsignature = Love_type_list.get_type "signature" []
let tstring = Love_type_list.get_type "string" []
let toperation = Love_type_list.get_type "operation" []
let tbytes = Love_type_list.get_type "bytes" []

let with_contract_type f ~sig_of_contract = function
  | [AContractType ct] -> f sig_of_contract ct
  | _ -> raise (InvariantBroken "Bad extended argument (expected ContractType)")

let with_type f ~sig_of_contract:_ = function
  | [AType t] -> f t
  | _ -> raise (InvariantBroken "Bad extended argument (expected Type)")

let with_type_pair f ~sig_of_contract:_ = function
  | [AType t1; AType t2] -> f t1 t2
  | _ -> raise (InvariantBroken "Bad extended argument (expected Type pair)")

let without_args f ~sig_of_contract:_ = function
  | [] -> f ()
  | _ -> raise (InvariantBroken "Bad extended argument (expected none)")

let init () =

  let tv tv_name tv_traits = { tv_name; tv_traits } in
  let a_comp = tv "_a" Love_type.comparable in let ta_comp = TVar a_comp in
  let b_comp = tv "_b" Love_type.comparable in let tb_comp = TVar b_comp in
  let c_comp = tv "_c" Love_type.comparable in let tc_comp = TVar c_comp in
  let a = tv "_a" Love_type.default_trait in let ta = TVar a in
  let b = tv "_b" Love_type.default_trait in let tb = TVar b in
  let c = tv "_c" Love_type.default_trait in let tc = TVar c in
  let d = tv "_d" Love_type.default_trait in let td = TVar d in

  let consume_gas = Love_prim_interp.consume_gas in

  let module G = Love_gas.Cost_of in

  (* Comparisons *) (* May raise Uncomparable *)

  register
    {
      prim_name = "compare" ;
      prim_id = 0 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> ta_comp @=> tint)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v1; v2] ->
           consume_gas ctxt (G.compare v1 v2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.of_int (Value.compare v1 v2)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "=" ;
      prim_id = 1 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> ta_comp @=> tbool)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v1; v2] ->
           consume_gas ctxt (G.compare v1 v2) >>=? fun ctxt ->
           return (ctxt, VBool Compare.Int.(Value.compare v1 v2 = 0))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "<>" ;
      prim_id = 2 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> ta_comp @=> tbool)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v1; v2] ->
           consume_gas ctxt (G.compare v1 v2) >>=? fun ctxt ->
           return (ctxt, VBool Compare.Int.(Value.compare v1 v2 <> 0))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "<" ;
      prim_id = 3 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> ta_comp @=> tbool)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v1; v2] ->
           consume_gas ctxt (G.compare v1 v2) >>=? fun ctxt ->
           return (ctxt, VBool Compare.Int.(Value.compare v1 v2 < 0))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "<=" ;
      prim_id = 4 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> ta_comp @=> tbool)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v1; v2] ->
           consume_gas ctxt (G.compare v1 v2) >>=? fun ctxt ->
           return (ctxt, VBool Compare.Int.(Value.compare v1 v2 <= 0))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = ">" ;
      prim_id = 5 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> ta_comp @=> tbool)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v1; v2] ->
           consume_gas ctxt (G.compare v1 v2) >>=? fun ctxt ->
           return (ctxt, VBool Compare.Int.(Value.compare v1 v2 > 0))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = ">=" ;
      prim_id = 6 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> ta_comp @=> tbool)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v1; v2] ->
           consume_gas ctxt (G.compare v1 v2) >>=? fun ctxt ->
           return (ctxt, VBool Compare.Int.(Value.compare v1 v2 >= 0))
       | _ -> bad_arguments p args
    ) ;

  (* Arithmetic on Integers *)

  register
    {
      prim_name = "+" ;
      prim_id = 7 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] ->
           consume_gas ctxt (G.add i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.add i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-" ;
      prim_id = 8 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] ->
           consume_gas ctxt (G.sub i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.sub i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*" ;
      prim_id = 9 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] ->
           consume_gas ctxt (G.mul i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.mul i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "/" ;
      prim_id = 10 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tint @=> tint @=> toption (TTuple [tint; tnat])) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] ->
           consume_gas ctxt (G.div i1 i2) >>=? fun ctxt ->
           begin try
               let q, r = Z.ediv_rem i1 i2 in
               return (ctxt, Value.some (VTuple [VInt q; VNat r]))
             with Division_by_zero -> return (ctxt, Value.none ())
           end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "~-" ;
      prim_id = 11 ;
      prim_kind = PrimPrefix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i] ->
           consume_gas ctxt (G.neg i) >>=? fun ctxt ->
           return (ctxt, VInt (Z.neg i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "~+" ;
      prim_id = 12 ;
      prim_kind = PrimPrefix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i] -> return (ctxt, VInt i)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "abs" ;
      prim_id = 13 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tnat) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i] ->
           consume_gas ctxt (G.abs i) >>=? fun ctxt ->
           return (ctxt, VInt (Z.abs i))
       | _ -> bad_arguments p args
    ) ;

  (* Arithmetic on Naturals *)

  register
    {
      prim_name = "++" ;
      prim_id = 14 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tnat) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.add i1 i2) >>=? fun ctxt ->
           return (ctxt, VNat (Z.add i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-+" ;
      prim_id = 15 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.sub i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.sub i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*+" ;
      prim_id = 16 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tnat) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.mul i1 i2) >>=? fun ctxt ->
           return (ctxt, VNat (Z.mul i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "/+" ;
      prim_id = 17 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tnat @=> tnat @=> toption (TTuple [tnat; tnat])) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.div i1 i2) >>=? fun ctxt ->
           begin try
               let q, r = Z.ediv_rem i1 i2 in
               return (ctxt, Value.some (VTuple [VNat q; VNat r]))
             with Division_by_zero -> return (ctxt, Value.none ())
           end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "~-+" ;
      prim_id = 18 ;
      prim_kind = PrimPrefix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tint) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i] ->
           consume_gas ctxt (G.neg i) >>=? fun ctxt ->
           return (ctxt, VInt (Z.neg i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "~++" ;
      prim_id = 19 ;
      prim_kind = PrimPrefix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i] -> return (ctxt, VNat i)
       | _ -> bad_arguments p args
    ) ;

  (* Arithmetic on Integers/Naturals *)

  register
    {
      prim_name = "++!" ;
      prim_id = 20 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VInt i2] ->
           consume_gas ctxt (G.add i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.add i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "+!+" ;
      prim_id = 21 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tnat @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VNat i2] ->
           consume_gas ctxt (G.add i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.add i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-+!" ;
      prim_id = 22 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VInt i2] ->
           consume_gas ctxt (G.sub i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.sub i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-!+" ;
      prim_id = 23 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tnat @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VNat i2] ->
           consume_gas ctxt (G.sub i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.sub i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*+!" ;
      prim_id = 24 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VInt i2] ->
           consume_gas ctxt (G.mul i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.mul i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*!+" ;
      prim_id = 25 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tnat @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VNat i2] ->
           consume_gas ctxt (G.mul i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.mul i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "/+!" ;
      prim_id = 26 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tnat @=> tint @=> toption (TTuple [tint; tnat])) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VInt i2] ->
           consume_gas ctxt (G.div i1 i2) >>=? fun ctxt ->
           begin try
               let q, r = Z.ediv_rem i1 i2 in
               return (ctxt, Value.some (VTuple [VInt q; VNat r]))
             with Division_by_zero -> return (ctxt, Value.none ())
           end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "/!+" ;
      prim_id = 27 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tint @=> tnat @=> toption (TTuple [tint; tnat])) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VNat i2] ->
           consume_gas ctxt (G.div i1 i2) >>=? fun ctxt ->
           begin try
               let q, r = Z.ediv_rem i1 i2 in
               return (ctxt, Value.some (VTuple [VInt q; VNat r]))
             with Division_by_zero -> return (ctxt, Value.none ())
           end
       | _ -> bad_arguments p args
    ) ;

  (* Arithmetic on Duns *)

  register
    {
      prim_name = "+$" ;
      prim_id = 28 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tdun @=> tdun @=> tdun) ;
      prim_arity = 2 ;
    }
    G.int64_op
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VDun d1; VDun d2] ->
           Lwt.return (Tez.(d1 +? d2) >|? fun res -> (ctxt, VDun res))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-$" ;
      prim_id = 29 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tdun @=> tdun @=> toption tdun) ;
      prim_arity = 2 ;
    }
    G.int64_op
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VDun d1; VDun d2] ->
           if Compare.Int.(Tez.compare d1 d2 < 0) then
             return (ctxt, Value.none ())
           else Lwt.return (Tez.(d1 -? d2)
                            >|? fun res -> (ctxt, Value.some (VDun res)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "/$" ;
      prim_id = 30 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tdun @=> tdun @=> toption (TTuple [tint; tdun])) ;
      prim_arity = 2 ;
    }
    Gas.(G.int64_op +@ G.int64_op +@ G.int64_to_z)
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VDun d1; VDun d2] -> (* May raise Z.Overflow *)
           let d1, d2 = Tez.to_mutez d1, Tez.to_mutez d2 in
           begin try
               let q, r = Int64.div d1 d2, Int64.rem d1 d2 in
               begin match Tez.of_mutez r with
                 | None -> return (ctxt, Value.none ())
                 | Some r -> return
                     (ctxt, Value.some (VTuple [VInt (Z.of_int64 q); VDun r]))
               end
             with Division_by_zero -> return (ctxt, Value.none ())
           end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*$!" ;
      prim_id = 31 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tdun @=> tint @=> toption tdun) ;
      prim_arity = 2 ;
    }
    Gas.(G.int64_op +@ G.z_to_int64)
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VDun d; VInt i] -> (* May raise Z.Overflow *)
           if Compare.Int.(Z.compare i Z.zero < 0) then
             return (ctxt, Value.none ())
           else Lwt.return (Tez.(d *? (Z.to_int64 i))
                            >|? fun res -> (ctxt, Value.some (VDun res)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*!$" ;
      prim_id = 32 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tdun @=> toption tdun) ;
      prim_arity = 2 ;
    }
    Gas.(G.int64_op +@ G.z_to_int64)
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i; VDun d] -> (* May raise Z.Overflow *)
           if Compare.Int.(Z.compare i Z.zero < 0) then
             return (ctxt, Value.none ())
           else Lwt.return (Tez.(d *? (Z.to_int64 i))
                            >|? fun res -> (ctxt, Value.some (VDun res)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "/$!" ;
      prim_id = 33 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tdun @=> tint @=> toption (TTuple [tdun; tdun])) ;
      prim_arity = 2 ;
    }
    Gas.(G.int64_op +@ G.int64_op +@ G.z_to_int64)
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VDun d; VInt i] -> (* May raise Z.Overflow *)
           if Compare.Int.(Z.compare i Z.zero < 0) then
             return (ctxt, Value.none ())
           else
             let d, i = Tez.to_mutez d, Z.to_int64 i in
             begin try
                 let q, r = Int64.div d i, Int64.rem d i in
                 begin match Tez.of_mutez q, Tez.of_mutez r with
                   | None, _ | _, None -> return (ctxt, Value.none ())
                   | Some q, Some r ->
                       return (ctxt, Value.some (VTuple [VDun q; VDun r]))
                 end
               with Division_by_zero -> return (ctxt, Value.none ())
             end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*$+" ;
      prim_id = 34 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tdun @=> tnat @=> tdun) ;
      prim_arity = 2 ;
    }
    Gas.(G.int64_op +@ G.z_to_int64)
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VDun d; VNat i] -> (* May raise Z.Overflow *)
           Lwt.return (Tez.(d *? (Z.to_int64 i))
                       >|? fun res -> (ctxt, VDun res))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "*+$" ;
      prim_id = 35 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tdun @=> tdun) ;
      prim_arity = 2 ;
    }
    Gas.(G.int64_op +@ G.z_to_int64)
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i; VDun d] -> (* May raise Z.Overflow *)
           Lwt.return (Tez.(d *? (Z.to_int64 i))
                       >|? fun res -> (ctxt, VDun res))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "/$+" ;
      prim_id = 36 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tdun @=> tnat @=> toption (TTuple [tdun; tdun])) ;
      prim_arity = 2 ;
    }
    Gas.(G.int64_op +@ G.int64_op +@ G.z_to_int64)
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VDun d; VNat i] -> (* May raise Z.Overflow *)
           if Compare.Int.(Z.compare i Z.zero < 0) then
             return (ctxt, Value.none ())
           else
             let d, i = Tez.to_mutez d, Z.to_int64 i in
             begin try
                 let q, r = Int64.div d i, Int64.rem d i in
                 begin match Tez.of_mutez q, Tez.of_mutez r with
                   | None, _ | _, None -> return (ctxt, Value.none ())
                   | Some q, Some r ->
                       return (ctxt, Value.some (VTuple [VDun q; VDun r]))
                 end
               with Division_by_zero -> return (ctxt, Value.none ())
             end
       | _ -> bad_arguments p args
    ) ;

  (* Arithmetic on Timestamps *)

  register
    {
      prim_name = "+:!" ;
      prim_id = 37 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> ttimestamp @=> tint @=> ttimestamp) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VTimestamp t; VInt i] ->
           consume_gas ctxt (G.add_timestamp t i) >>=? fun ctxt ->
           let i = Script_int.of_zint i in
           return (ctxt, VTimestamp (Script_timestamp.add_delta t i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "+!:" ;
      prim_id = 38 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> ttimestamp @=> ttimestamp) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i; VTimestamp t] ->
           consume_gas ctxt (G.add_timestamp t i) >>=? fun ctxt ->
           let i = Script_int.of_zint i in
           return (ctxt, VTimestamp (Script_timestamp.add_delta t i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-:!" ;
      prim_id = 39 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> ttimestamp @=> tint @=> ttimestamp) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VTimestamp t; VInt i] ->
           consume_gas ctxt (G.sub_timestamp t i) >>=? fun ctxt ->
           let i = Script_int.of_zint i in
           return (ctxt, VTimestamp (Script_timestamp.sub_delta t i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "+:+" ;
      prim_id = 40 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> ttimestamp @=> tnat @=> ttimestamp) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VTimestamp t; VNat i] ->
           consume_gas ctxt (G.add_timestamp t i) >>=? fun ctxt ->
           let i = Script_int.of_zint i in
           return (ctxt, VTimestamp (Script_timestamp.add_delta t i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "++:" ;
      prim_id = 41 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> ttimestamp @=> ttimestamp) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i; VTimestamp t] ->
           consume_gas ctxt (G.add_timestamp t i) >>=? fun ctxt ->
           let i = Script_int.of_zint i in
           return (ctxt, VTimestamp (Script_timestamp.add_delta t i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-:+" ;
      prim_id = 42 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> ttimestamp @=> tnat @=> ttimestamp) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VTimestamp t; VNat i] ->
           consume_gas ctxt (G.sub_timestamp t i) >>=? fun ctxt ->
           let i = Script_int.of_zint i in
           return (ctxt, VTimestamp (Script_timestamp.sub_delta t i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "-:" ;
      prim_id = 43 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> ttimestamp @=> ttimestamp @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VTimestamp t1; VTimestamp t2] ->
           consume_gas ctxt (G.diff_timestamps t1 t2) >>=? fun ctxt ->
           let i = Script_timestamp.diff t1 t2 in
           return (ctxt, VInt (Script_int.to_zint i))
       | _ -> bad_arguments p args
    ) ;

  (* Boolean operators *)

  register
    {
      prim_name = "&&" ;
      prim_id = 44 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbool @=> tbool @=> tbool) ;
      prim_arity = 2 ;
    }
    G.bool_binop
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBool b1; VBool b2] -> return (ctxt, VBool (b1 && b2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "||" ;
      prim_id = 45 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbool @=> tbool @=> tbool) ;
      prim_arity = 2 ;
    }
    G.bool_binop
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBool b1; VBool b2] -> return (ctxt, VBool (b1 || b2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "|&" ;
      prim_id = 46 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbool @=> tbool @=> tbool) ;
      prim_arity = 2 ;
    }
    G.bool_binop
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBool b1; VBool b2] -> return (ctxt, VBool Compare.Bool.(b1 <> b2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "not" ;
      prim_id = 47 ;
      prim_kind = PrimPrefix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbool @=> tbool) ;
      prim_arity = 1 ;
    }
    G.bool_unop
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBool b] -> return (ctxt, VBool (not b))
       | _ -> bad_arguments p args
    ) ;

  (* Bitwise operators (Integer) *)

  register
    {
      prim_name = "land" ;
      prim_id = 48 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] ->
           consume_gas ctxt (G.logand i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.logand i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "lor" ;
      prim_id = 49 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] ->
           consume_gas ctxt (G.logor i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.logor i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "lxor" ;
      prim_id = 50 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] ->
           consume_gas ctxt (G.logxor i1 i2) >>=? fun ctxt ->
           return (ctxt, VInt (Z.logxor i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "lnot" ;
      prim_id = 51 ;
      prim_kind = PrimPrefix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i] ->
           consume_gas ctxt (G.lognot i) >>=? fun ctxt ->
           return (ctxt, VInt (Z.lognot i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "lsl" ;
      prim_id = 52 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] -> (* If negative : shift in opposite direction *)
           consume_gas ctxt (G.shift_left i1 i2) >>=? fun ctxt ->
           let i2 = Z.to_int i2 in (* May raise Z.Overflow *)
           if Compare.Int.(i2 >= 0) then
             return (ctxt, VInt (Z.shift_left i1 i2))
           else return  (ctxt, VInt (Z.shift_right i1 (abs i2)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "lsr" ;
      prim_id = 53 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> tint @=> tint) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VInt i1; VInt i2] -> (* If negative : shift in opposite direction *)
           consume_gas ctxt (G.shift_right i1 i2) >>=? fun ctxt ->
           let i2 = Z.to_int i2 in (* May raise Z.Overflow *)
           if Compare.Int.(i2 >= 0) then
             return (ctxt, VInt (Z.shift_right i1 i2))
           else return (ctxt, VInt (Z.shift_left i1 (abs i2)))
       | _ -> bad_arguments p args
    ) ;

  (* Bitwise operators (Natural) *)

  register
    {
      prim_name = "nland" ;
      prim_id = 54 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tnat) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.logand i1 i2) >>=? fun ctxt ->
           return (ctxt, VNat (Z.logand i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "nlor" ;
      prim_id = 55 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tnat) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.logor i1 i2) >>=? fun ctxt ->
           return (ctxt, VNat (Z.logor i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "nlxor" ;
      prim_id = 56 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tnat) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.logxor i1 i2) >>=? fun ctxt ->
           return (ctxt, VNat (Z.logxor i1 i2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "nlsl" ;
      prim_id = 57 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tnat) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.shift_left i1 i2) >>=? fun ctxt ->
           let i2 = Z.to_int i2 in (* May raise Z.Overflow *)
           return (ctxt, VNat (Z.shift_left i1 i2))
               (* i1 >= 0, hence result >= 0 *)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "nlsr" ;
      prim_id = 58 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tnat @=> tnat) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2] ->
           consume_gas ctxt (G.shift_right i1 i2) >>=? fun ctxt ->
           let i2 = Z.to_int i2 in (* May raise Z.Overflow *)
           return (ctxt, VNat (Z.shift_right i1 i2))
               (* i1 >= 0, hence result >= 0 *)
       | _ -> bad_arguments p args
    ) ;

  (* List primitives *)

  register
    {
      prim_name = "List.length" ;
      prim_id = 59 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> TForall (a, tlist ta @=> tnat)) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VList l] ->
           let i = List.length l in
           consume_gas ctxt (G.list_size i) >>=? fun ctxt ->
           return (ctxt, VNat (Z.of_int i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "List.cons" ;
      prim_id = 60 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, ta @=> tlist ta @=> tlist ta)) ;
      prim_arity = 2 ;
    }
    G.list_cons
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v; VList l] -> return (ctxt, VList (v :: l))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "List.rev" ;
      prim_id = 61 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> TForall (a, tlist ta @=> tlist ta)) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VList l] ->
           consume_gas ctxt (G.list_rev (List.length l)) >>=? fun ctxt ->
           return (ctxt, VList (List.rev l))
       | _ -> bad_arguments p args
    ) ;

  let concat_prim, concat_gas, concat_code =
    {
      prim_name = "List.concat" ;
      prim_id = 62 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, tlist ta @=> tlist ta @=> tlist ta)) ;
      prim_arity = 2 ;
    },
    Gas.free,
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VList l] ->
           let l = List.map (function
             | VList l -> l
             | _ -> raise (InvariantBroken "Invalid List.concat argument")) l
           in
           let l = List.concat l in
           consume_gas ctxt (G.list_concat (List.length l)) >>=? fun ctxt ->
           return (ctxt, VList l)
       | _ -> bad_arguments p args
    )
  in

  register
    concat_prim
    concat_gas
    concat_code ;

  register
    {
      concat_prim with
      prim_name = "@";
      prim_id = 63;
      prim_kind = PrimInfix
    }
    concat_gas
    concat_code ;

  register
    {
      prim_name = "List.iter" ;
      prim_id = 64 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, (ta @=> tunit) @=> tlist ta @=> tunit)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VList l] ->
           fold_left_s (fun ctxt v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v])
             >>|? fun (ctxt, _v') -> ctxt) ctxt l
           >>|? fun ctxt -> (ctxt, VUnit)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "List.map" ;
      prim_id = 65 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, TForall (b, (ta @=> tb) @=> tlist ta @=> tlist tb))) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VList l] ->
           fold_left_s (fun (ctxt, l) v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v])
             >>|? fun (ctxt, v') -> (ctxt, v' :: l)) (ctxt, []) l
           >>|? fun (ctxt, l) -> (ctxt, VList (List.rev l))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "List.fold" ;
      prim_id = 66 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, TForall (b,
              (ta @=> tb @=> tb) @=> tlist ta @=> tb @=> tb))) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VList l; a] ->
           fold_left_s (fun (ctxt, a) v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v; a])) (ctxt, a) l
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "List.map_fold" ;
      prim_id = 67 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, TForall (b, TForall (c,
              (ta @=> tb @=> TTuple [tc; tb])
              @=> tlist ta @=> tb @=> TTuple [tlist tc; tb])))) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VList l; a] ->
           fold_left_s (fun (ctxt, (l, a)) v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v; a]) >>|? function
             | ctxt, VTuple [v'; a'] -> (ctxt, (v' :: l, a'))
             | _ -> raise (InvariantBroken "Invalid List.map_fold function")
           ) (ctxt, ([], a)) l
           >>|? fun (ctxt, (l, a)) -> (ctxt, VTuple [VList (List.rev l); a])
       | _ -> bad_arguments p args
    ) ;

  (* Set primitives *)

  register
    {
      prim_name = "Set.empty" ;
      prim_id = 68 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> TForall (a_comp, tset ta_comp)) ;
      prim_arity = 0 ;
    }
    G.empty_set
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [] -> return (ctxt, VSet (ValueSet.empty))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.cardinal" ;
      prim_id = 69 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, tset ta_comp @=> tnat)) ;
      prim_arity = 1 ;
    }
    G.set_size
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VSet s] -> return (ctxt, VNat (Z.of_int (ValueSet.cardinal s)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.add" ;
      prim_id = 70 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> tset ta_comp @=> tset ta_comp)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v; VSet s] ->
             consume_gas ctxt (G.set_add v s) >>=? fun ctxt ->
             return (ctxt, VSet (ValueSet.add v s))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.remove" ;
      prim_id = 71 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> tset ta_comp @=> tset ta_comp)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v; VSet s] ->
             consume_gas ctxt (G.set_remove v s) >>=? fun ctxt ->
             return (ctxt, VSet (ValueSet.remove v s))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.mem" ;
      prim_id = 72 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, ta_comp @=> tset ta_comp @=> tbool)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v; VSet s] ->
             consume_gas ctxt (G.set_mem v s) >>=? fun ctxt ->
             return (ctxt, VBool (ValueSet.mem v s))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.iter" ;
      prim_id = 73 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, (ta_comp @=> tunit) @=> tset ta_comp @=> tunit)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VSet s] ->
           fold_left_s (fun ctxt v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v])
             >>|? fun (ctxt, _v') -> ctxt) ctxt (ValueSet.elements s)
           >>|? fun ctxt -> (ctxt, VUnit)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.map" ;
      prim_id = 74 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b_comp,
              (ta_comp @=> tb_comp) @=> tset ta_comp @=> tset tb_comp))) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VSet s; a] ->
           fold_left_s (fun (ctxt, a) v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v; a])
           ) (ctxt, a) (ValueSet.elements s)
           >>|? fun (ctxt, a) -> (ctxt, a)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.fold" ;
      prim_id = 75 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              (ta_comp @=> tb @=> tb) @=> tset ta_comp @=> tb @=> tb))) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VSet s; a] ->
           fold_left_s (fun (ctxt, a) v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v; a])
           ) (ctxt, a) (ValueSet.elements s)
           >>|? fun (ctxt, a) -> (ctxt, a)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Set.map_fold" ;
      prim_id = 76 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b, TForall (c_comp,
              (ta_comp @=> tb @=> TTuple [tc_comp; tb])
              @=> tset ta_comp @=> tb @=> TTuple [tset tc_comp; tb])))) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VSet s; a] ->
           fold_left_s (fun (ctxt, (s, a)) v ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v; a]) >>|? function
             | ctxt, VTuple [v'; a'] -> (ctxt, (ValueSet.add v' s, a'))
             | _ -> raise (InvariantBroken "Invalid Set.map_fold function")
           ) (ctxt, (ValueSet.empty, a)) (ValueSet.elements s)
           >>|? fun (ctxt, (s, a)) -> (ctxt, VTuple [VSet s; a])
       | _ -> bad_arguments p args
    ) ;

  (* Map primitives *)

  register
    {
      prim_name = "Map.empty" ;
      prim_id = 77 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b, tmap (ta_comp, tb)))) ;
      prim_arity = 0 ;
    }
    G.empty_map
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [] -> return (ctxt, VMap (ValueMap.empty))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.cardinal" ;
      prim_id = 78 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b, tmap (ta_comp, tb) @=> tnat))) ;
      prim_arity = 1 ;
    }
    G.map_size
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VMap m] -> return (ctxt, VNat (Z.of_int (ValueMap.cardinal m)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.add" ;
      prim_id = 79 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              ta_comp @=> tb @=> tmap (ta_comp, tb) @=> tmap (ta_comp, tb)))) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; v; VMap m] ->
           consume_gas ctxt (G.map_add k v m) >>=? fun ctxt ->
           return (ctxt, VMap (ValueMap.add k v m))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.remove" ;
      prim_id = 80 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              ta_comp @=> tmap (ta_comp, tb) @=> tmap (ta_comp, tb)))) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; VMap m] ->
           consume_gas ctxt (G.map_remove k m) >>=? fun ctxt ->
           return (ctxt, VMap (ValueMap.remove k m))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.mem" ;
      prim_id = 81 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              ta_comp @=> tmap (ta_comp, tb) @=> tbool))) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; VMap m] ->
           consume_gas ctxt (G.map_mem k m) >>=? fun ctxt ->
           return (ctxt, VBool (ValueMap.mem k m))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.find" ;
      prim_id = 82 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              ta_comp @=> tmap (ta_comp, tb) @=> toption tb))) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; VMap m] ->
           consume_gas ctxt (G.map_get k m) >>=? fun ctxt ->
           begin match ValueMap.find_opt k m with
             | None -> return (ctxt, Value.none ())
             | Some v -> return (ctxt, Value.some v)
           end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.iter" ;
      prim_id = 83 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              (ta_comp @=> tb @=> tunit) @=> tmap (ta_comp, tb) @=> tunit))) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VMap m] ->
           fold_left_s (fun ctxt (k, v) ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [k; v])
             >>|? fun (ctxt, _v') -> ctxt) ctxt (ValueMap.bindings m)
           >>|? fun ctxt -> (ctxt, VUnit)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.map" ;
      prim_id = 84 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b, TForall (c,
              (ta_comp @=> tb @=> tc)
              @=> tmap (ta_comp, tb) @=> tmap (ta_comp, tc))))) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VMap m] ->
           fold_left_s (fun (ctxt, m) (k, v) ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [k; v])
             >>|? fun (ctxt, v') -> (ctxt, ValueMap.add k v' m)
           ) (ctxt, ValueMap.empty) (ValueMap.bindings m)
           >>|? fun (ctxt, m) -> (ctxt, VMap m)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.fold" ;
      prim_id = 85 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b, TForall (c,
              (ta_comp @=> tb @=> tc @=> tc)
              @=> tmap (ta_comp, tb) @=> tc @=> tc)))) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VMap m; a] ->
           fold_left_s (fun (ctxt, a) (k, v) ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [k; v; a])
           ) (ctxt, a) (ValueMap.bindings m)
           >>|? fun (ctxt, a) -> (ctxt, a)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Map.map_fold" ;
      prim_id = 86 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b, TForall (c, TForall (d,
              (ta_comp @=> tb @=> tc @=> TTuple [td; tc])
              @=> tmap (ta_comp, tb) @=> tc
              @=> TTuple [tmap (ta_comp, td); tc]))))) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; VMap m; a] ->
           fold_left_s (fun (ctxt, (m, a)) (k, v) ->
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [k; v; a]) >>|? function
             | ctxt, VTuple [v'; a'] -> (ctxt, (ValueMap.add k v' m, a'))
             | _ -> raise (InvariantBroken "Invalid Map.map_fold function")
           ) (ctxt, (ValueMap.empty, a)) (ValueMap.bindings m)
           >>|? fun (ctxt, (m, a)) -> (ctxt, VTuple [VMap m; a])
       | _ -> bad_arguments p args
    ) ;

  (* BigMap primitives *)

  register
    {
      prim_name = "BigMap.empty" ;
      prim_id = 87 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [ADType; ADType] ;
      prim_type = with_type_pair (fun tk tv -> tbigmap (tk, tv)) ;
      prim_arity = 0 ;
    }
    (G.big_map_empty tunit tunit)
    (fun _apply_value ctxt env p xl args ->
       match xl, args with
       | [AType tk; AType tv], [] ->
           Love_translator.make_type_absolute ctxt env tk >>=? fun (ctxt, tk) ->
           Love_translator.make_type_absolute ctxt env tv >>|? fun (ctxt, tv) ->
           ctxt, VBigMap (Love_prim_interp.empty_big_map tk tv)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "BigMap.add" ;
      prim_id = 88 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, (TForall (b,
              ta_comp @=> tb @=> tbigmap (ta_comp, tb)
              @=> tbigmap (ta_comp, tb))))) ;
      prim_arity = 3 ;
    }
    (G.big_map_update VUnit (Some VUnit) ())
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; v; VBigMap bm] ->
           Love_prim_interp.big_map_add ctxt bm k v
           >>|? fun (ctxt, bm) -> ctxt, VBigMap bm
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "BigMap.remove" ;
      prim_id = 89 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, (TForall (b,
              ta_comp @=> tbigmap (ta_comp, tb) @=> tbigmap (ta_comp, tb))))) ;
      prim_arity = 2 ;
    }
    (G.big_map_update VUnit None ())
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; VBigMap bm] ->
           Love_prim_interp.big_map_del ctxt bm k
           >>|? fun (ctxt, bm) -> ctxt, VBigMap bm
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "BigMap.mem" ;
      prim_id = 90 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              ta_comp @=> tbigmap (ta_comp, tb) @=> tbool))) ;
      prim_arity = 2 ;
    }
    (G.big_map_mem VUnit ())
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; VBigMap bm] ->
           (* also, consumes gas for computing hash key *)
           Love_prim_interp.big_map_mem ctxt bm k
           >>|? fun (ctxt, res) -> ctxt, VBool res
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "BigMap.find" ;
      prim_id = 91 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a_comp, TForall (b,
              ta_comp @=> tbigmap (ta_comp, tb) @=> toption tb))) ;
      prim_arity = 2 ;
    }
    (G.big_map_get VUnit ())
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [k; VBigMap bm] ->
           (* also, consumes gas for computing hash key *)
           Love_prim_interp.big_map_get ctxt bm k
       | _ -> bad_arguments p args
    ) ;

  (* Loop primitive *)

  register
    {
      prim_name = "Loop.loop" ;
      prim_id = 92 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, (ta @=> TTuple [tbool;ta]) @=> ta @=> ta)) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [f; v] ->
           let rec loop ctxt v =
             consume_gas ctxt G.loop_cycle >>=? fun ctxt ->
             apply_value ctxt env f (Val [v]) >>=? function
             | ctxt, VTuple [VBool true; v'] -> loop ctxt v'
             | ctxt, VTuple [VBool false; v'] -> return (ctxt, v')
             | _ -> raise (InvariantBroken "Invalid Loop.loop function")
           in
           loop ctxt v
       | _ -> bad_arguments p args
    ) ;

  (* String primitives *)

  register
    {
      prim_name = "String.length" ;
      prim_id = 93 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tstring @=> tnat) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VString s] ->
           let i = String.length s in
           consume_gas ctxt (G.string i) >>=? fun ctxt ->
           return (ctxt, VNat (Z.of_int i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "String.concat" ;
      prim_id = 94 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tlist tstring @=> tstring) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VList l] ->
           map_s (function
             | VString s -> return s
             | _ -> raise (InvariantBroken "Bad String.concat argument")) l
           >>=? fun l ->
           let s = String.concat "" l in
           consume_gas ctxt (G.string (String.length s)) >>=? fun ctxt ->
           return (ctxt, VString s)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "^" ;
      prim_id = 95 ;
      prim_kind = PrimInfix ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tstring @=> tstring @=> tstring) ;
      prim_arity = 2 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VString s1; VString s2] ->
           consume_gas ctxt (G.string (String.length s1 + String.length s2))
           >>=? fun ctxt ->
           return (ctxt, VString (s1 ^ s2))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "String.slice" ;
      prim_id = 96 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tnat @=> tnat @=> tstring @=> toption tstring) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2; VString s] -> (* May raise Z.Overflow *)
           consume_gas ctxt (G.string (String.length s)) >>=? fun ctxt ->
           let res =
             try Value.some (VString (String.sub s (Z.to_int i1) (Z.to_int i2)))
             with Invalid_argument _ -> Value.none () in
           return (ctxt, res)
       | _ -> bad_arguments p args
    ) ;

  (* Bytes primitives *)

  register
    {
      prim_name = "Bytes.length" ;
      prim_id = 97 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbytes @=> tnat) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBytes b] ->
           let i = MBytes.length b in
           consume_gas ctxt (G.bytes i) >>=? fun ctxt ->
           return (ctxt, VNat (Z.of_int i))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Bytes.concat" ;
      prim_id = 98 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tlist tbytes @=> tbytes) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VList l] ->
           map_s (function
             | VBytes b -> return b
             | _ -> raise (InvariantBroken "Bad Bytes.concat argument")) l
           >>=? fun l ->
           let b = MBytes.concat "" l in
           consume_gas ctxt (G.bytes (MBytes.length b)) >>=? fun ctxt ->
           return (ctxt, VBytes b)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Bytes.slice" ;
      prim_id = 99 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tnat @=> tnat @=> tbytes @=> toption tbytes) ;
      prim_arity = 3 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VNat i1; VNat i2; VBytes b] -> (* May raise Z.Overflow *)
           consume_gas ctxt (G.string (MBytes.length b)) >>=? fun ctxt ->
           let res =
             try Value.some (VBytes (MBytes.sub b (Z.to_int i1) (Z.to_int i2)))
             with Invalid_argument _ -> Value.none () in
           return (ctxt, res)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Bytes.pack" ;
      prim_id = 100 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> TForall (a, TVar a @=> tbytes)) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [v] ->
           begin match Data_encoding.Binary.to_bytes
                         Love_encoding.Value.encoding v with
           | None -> raise (InvariantBroken "Can't encode value")
           | Some b ->
               (* 5 = Packed value, 1 = encoding version *)
               let b = MBytes.concat ""
                   [ MBytes.of_string "\005" ; MBytes.of_string "\001" ; b ] in
               consume_gas ctxt (G.bytes (MBytes.length b)) >>=? fun ctxt ->
               return (ctxt, VBytes b)
           end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Bytes.unpack" ;
      prim_id = 101 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [ADType] ;
      prim_type = with_type (fun ty -> tbytes @=> toption ty) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p xl args ->
       match xl, args with
       | [AType ty], [VBytes b] ->
           consume_gas ctxt (G.bytes (MBytes.length b)) >>=? fun ctxt ->
           if Compare.Int.(MBytes.length b >= 2) &&
              Compare.Int.(MBytes.get_uint8 b 0 = 0x05) then
             if Compare.Int.(MBytes.get_uint8 b 1 = 0x01) then
               let b = MBytes.sub b 2 (MBytes.length b - 2) in
               match Data_encoding.Binary.of_bytes
                       Love_encoding.Value.encoding b with
               | None -> return (ctxt, Value.none ())
               | Some v ->
                 try Love_typechecker.typecheck_value
                       (Love_tenv.empty (Contract []) ()) ctxt
                       { lettypes = []; body = ty } v >>=? fun ctxt ->
                   return (ctxt, Value.some v)
                 with _ -> return (ctxt, Value.none ())
             else raise (InvariantBroken "Unsupported encoding version")
           else return (ctxt, Value.none ())
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Bytes.hash" ;
      prim_id = 102 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbytes @=> tbytes) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBytes b] ->
           (* hash_data already consumes gas *)
           Love_prim_interp.hash_data ctxt (VBytes b) >>|? fun (ctxt, res) ->
           (ctxt, VBytes (Script_expr_hash.to_bytes res))
       | _ -> bad_arguments p args
    ) ;

  (* Cryptographic operations *)

  register
    {
      prim_name = "Crypto.blake2b" ;
      prim_id = 103 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbytes @=> tbytes) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBytes b] ->
           consume_gas ctxt (G.hash b 32) >>=? fun ctxt ->
           return (ctxt, VBytes (Raw_hashes.blake2b b))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Crypto.sha256" ;
      prim_id = 104 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbytes @=> tbytes) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBytes b] ->
           consume_gas ctxt (G.hash b 32) >>=? fun ctxt ->
           return (ctxt, VBytes (Raw_hashes.sha256 b))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Crypto.sha512" ;
      prim_id = 105 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tbytes @=> tbytes) ;
      prim_arity = 1 ;
    }
    Gas.free
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VBytes b] ->
           consume_gas ctxt (G.hash b 64) >>=? fun ctxt ->
           return (ctxt, VBytes (Raw_hashes.sha512 b))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Crypto.hash_key" ;
      prim_id = 106 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tkey @=> tkeyhash) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.hash_key
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VKey key] -> return (ctxt, VKeyHash (Signature.Public_key.hash key))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Crypto.check" ;
      prim_id = 107 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tkey @=> tsignature @=> tbytes @=> tbool) ;
      prim_arity = 3 ;
    }
    Love_gas.Cost_of.check_signature
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VKey key; VSignature sign; VBytes msg] ->
           return (ctxt, VBool (Signature.check key sign msg))
       | _ -> bad_arguments p args
    ) ;

  (* Contract interactions *)

  register
    {
      prim_name = "Contract.address" ;
      prim_id = 108 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TContractInstance unit_contract_named_type @=> taddress) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.address
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VContractInstance (_, a)] -> return (ctxt, VAddress a)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Contract.self" ;
      prim_id = 109 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [ADContractType] ;
      prim_type = with_contract_type (fun sig_of_contract ct ->
        let st = match ct with
          | StructType st -> st
          | ContractInstance c -> Anonymous (sig_of_contract c)
        in
        tunit @=> toption (TContractInstance st)) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.self
    (fun _apply_value ctxt env p xl args ->
       match xl, args with
       | [AContractType ct], [VUnit] ->
           Love_prim_interp.contract_at ctxt env ctxt.self ct
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Contract.at" ;
      prim_id = 110 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [ADContractType] ;
      prim_type = with_contract_type (fun sig_of_contract ct ->
        let st = match ct with
          | StructType st -> st
          | ContractInstance c -> Anonymous (sig_of_contract c)
        in
        taddress @=> toption (TContractInstance st)) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.contract
    (fun _apply_value ctxt env p xl args ->
       match xl, args with
       | [AContractType ct], [VAddress a] ->
           Love_prim_interp.contract_at ctxt env a ct
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Contract.call" ;
      prim_id = 111 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, tentrypoint ta @=> tdun @=> ta @=> toperation)) ;
      prim_arity = 3 ;
    }
    Love_gas.Cost_of.transfer
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VEntryPoint (destination, entrypoint); VDun amount; arg] ->
           let arg = Love_translator.inline_value ctxt arg in
           Love_prim_interp.collect_big_maps ctxt arg
           >>=? fun (to_duplicate, ctxt) ->
           Love_prim_interp.extract_big_map_diff
             ctxt arg ~to_duplicate ~to_update:Collections.ZSet.empty
             ~temporary:true >>=? fun (arg, big_map_diff, ctxt) ->
           let parameters = Some arg in
           let operation = Op.Transaction {
               amount; parameters; entrypoint; destination } in
           Lwt.return @@ fresh_internal_nonce ctxt.actxt
           >>|? fun (actxt, nonce) ->
           let op = Op.{ source = ctxt.self; operation; nonce = nonce } in
           ({ ctxt with actxt }, VOperation (op, big_map_diff))
       (* Note : can only fail if out of gas or more than 65535 internal ops *)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Contract.view" ;
      prim_id = 112 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          TForall (a, TForall (b, tview (ta, tb) @=> ta @=> tb))) ;
      prim_arity = 2 ;
    }
    Love_gas.Cost_of.view
    (fun apply_value ctxt env p _xl args ->
       match args with
       | [VView (destination, view); arg] ->
           Love_context.get_script ctxt destination >>=? fun (ctxt, script) ->
           begin match script with
             | None -> raise (InvariantBroken "Bad Contract.view contract")
             | Some (code, storage) ->
               match List.assoc_opt view code.content with
               | Some (VView { vview_code = v; _ }) ->
                 let fct = match v with
                   | VClosure _ as v -> v
                   | _ -> raise BadView in
                 apply_value ctxt env fct (Val [storage; arg])
               | _ -> raise BadView
           end
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Contract.create" ;
      prim_id = 113 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
        let ctype = TPackedStructure (
            Anonymous {
              unit_contract_sig with
              sig_content = [
                "storage", SType (SAbstract []);
                "__init_storage", SInit ta
              ] } )
        in
        TForall (a, toption tkeyhash @=> tdun @=> ctype
                    @=> ta @=> TTuple [toperation; taddress])) ;
      prim_arity = 4 ;
    }
    Love_gas.Cost_of.create_contract
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [deleg_opt; VDun credit; VPackedStructure (p, c); init_arg] ->
           if Love_value.LiveStructure.is_module c.root_struct then
             raise (InvariantBroken
                      "Contract.create requires a contract structure");
           Contract.fresh_contract_from_current_nonce ctxt.actxt
           >>=? fun (actxt, contract) ->
           let ctxt = { ctxt with actxt } in
           let to_path = Ident.create_id (Contract.to_b58check contract) in
           let code = Love_translator.rebase_contract
               ctxt ~from_path:p ~to_path c in
           let init_storage = init_arg in
           let init_storage = Love_translator.inline_value ctxt init_storage in
           Love_prim_interp.collect_big_maps ctxt init_storage
           >>=? fun (to_duplicate, ctxt) ->
           Love_prim_interp.extract_big_map_diff ctxt init_storage
             ~to_duplicate ~to_update:Collections.ZSet.empty
             ~temporary:true >>=? fun (init_storage, big_map_diff, ctxt) ->
           let delegate = match deleg_opt with
             | VConstr ("None", []) -> None
             | VConstr ("Some", [VKeyHash kh]) -> Some kh
             | _ -> raise (InvariantBroken "Bad Contract.create argument") in
           let operation = Op.Origination {
               delegate; script = (init_storage, code);
               credit; preorigination = Some contract } in
           Lwt.return @@ fresh_internal_nonce actxt >>|? fun (actxt, nonce) ->
           let op = Op.{ source = ctxt.self; operation; nonce } in
           ({ ctxt with actxt },
            VTuple [VOperation (op, big_map_diff); VAddress contract])
       (* Note : can fail if out of gas or origination nonce
          not initialized or more then 65535 internal ops *)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Contract.set_delegate" ;
      prim_id = 114 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> toption tkeyhash @=> toperation) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.set_delegate
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [deleg_opt] ->
           let delegate = match deleg_opt with
             | VConstr ("None", []) -> None
             | VConstr ("Some", [VKeyHash kh]) -> Some kh
             | _ ->
                 raise (InvariantBroken "Bad Contract.set_delegate argument") in
           let operation = Op.Delegation delegate in
           Lwt.return @@ fresh_internal_nonce ctxt.actxt
           >>|? fun (actxt, nonce) ->
           let op = Op.{ source = ctxt.self; operation; nonce = nonce } in
           ({ ctxt with actxt }, VOperation (op, None))
       (* Note : can only fail if out of gas or more than 65535 internal ops *)
       | _ -> bad_arguments p args
    ) ;

  (* Account interactions *)

  register
    {
      prim_name = "Account.default" ;
      prim_id = 115 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          tkeyhash @=> TContractInstance (
            Anonymous { unit_contract_sig with
                        sig_content = [ "default", SEntry tunit ] })) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.implicit_account
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VKeyHash kh] ->
           return (ctxt, VContractInstance (Love_type.unit_contract_sig,
                                            Contract.implicit_contract kh))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Account.transfer" ;
      prim_id = 116 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> taddress @=> tdun @=> toperation) ;
      prim_arity = 2 ;
    }
    Love_gas.Cost_of.transfer
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VAddress destination; VDun amount] ->
           let operation = Op.Transaction {
             amount; parameters = None;
             entrypoint = "default"; destination } in
           Lwt.return @@ fresh_internal_nonce ctxt.actxt
           >>|? fun (actxt, nonce) ->
           let op = Op.{ source = ctxt.self; operation; nonce = nonce } in
           ({ ctxt with actxt }, VOperation (op, None))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Account.balance" ;
      prim_id = 117 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> taddress @=> tdun) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.balance
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VAddress a] ->
           Contract.get_balance ctxt.actxt a >>|? fun balance ->
           (ctxt, VDun balance)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Account.get_info" ;
      prim_id = 118 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          taddress @=> TTuple [tint; tbool; toption taddress; tlist taddress]) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.getinfo
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VAddress target] ->
           begin match Contract.is_implicit target with
             | None -> return None
             | Some delegate -> Roll.Delegate.get_maxrolls ctxt.actxt delegate
           end >>=? fun maxrolls ->
           let maxrolls = match maxrolls with
             | None -> Value.none ()
             | Some i -> Value.some (VInt (Z.abs (Z.of_int i))) in
           Contract.get_delegation ctxt.actxt target >>=? fun deleg ->
           Contract.get_admin ctxt.actxt target >>=? fun admin ->
           let admin = match admin with
             | Some a -> Value.some (VAddress a)
             | None -> Value.none () in
           Contract.get_whitelist ctxt.actxt target >>= fun wl ->
           let wl = List.map (fun v -> VAddress v) wl in
           return (ctxt, VTuple [maxrolls; VBool deleg; admin; VList wl])
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Account.manage" ;
      prim_id = 119 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () ->
          taddress @=> toption tint @=> tbool @=> toption taddress
          @=> tlist taddress @=> toption toperation) ;
      prim_arity = 5 ;
    }
    Love_gas.Cost_of.manage
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VAddress target; maxrolls_opt; VBool deleg;
          admin_opt; VList white_list] ->
           let maxrolls = match maxrolls_opt with
             | VConstr ("None", []) -> Some (None)
             | VConstr ("Some", [VInt maxrolls]) ->
                 Some (Some (Z.to_int maxrolls))
             | _ -> raise (InvariantBroken "Invalid maxrolls: not an int")
           in
           let admin = match admin_opt with
             | VConstr ("None", []) -> Some (None)
             | VConstr ("Some", [VAddress a]) -> Some (Some a)
             | _ -> raise (InvariantBroken "Invalid admin: not an address")
           in
           let white_list = Some (List.map (function
               | VAddress a -> a
               | _ -> raise (InvariantBroken
                               "Invalid white list: not an address list")
             ) white_list)
           in
           let delegation = Some deleg in
           let target = match Contract.is_implicit target with
             | Some target_pkh -> Some target_pkh
             | None -> None
           in
           let operation = Op.Dune_manage_account {
               target; maxrolls; admin; white_list; delegation } in
           Lwt.return @@ fresh_internal_nonce ctxt.actxt
           >>|? fun (actxt, nonce) ->
           let op = Op.{ source = ctxt.self; operation; nonce = nonce } in
           ({ ctxt with actxt }, Value.some (VOperation (op, None)))
       | _ -> bad_arguments p args
    ) ;

  (* General information *)

  register
    {
      prim_name = "Current.balance" ;
      prim_id = 120 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> tdun) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.now
    (fun _apply_value ctxt _env p _xl args ->
       match args with
       | [VUnit] ->
           Contract.get_balance ctxt.actxt ctxt.self >>|? fun balance ->
           (ctxt, VDun balance)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.time" ;
      prim_id = 121 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> ttimestamp) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.now
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] -> (ctxt, VTimestamp (Script_timestamp.now ctxt.actxt))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.amount" ;
      prim_id = 122 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> tdun) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.amount
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] -> (ctxt, VDun ctxt.amount)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.gas" ;
      prim_id = 123 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> tnat) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.steps_to_quota
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] ->
           let steps = match Gas.level ctxt.actxt with
             | Unaccounted -> Z.of_int 99999999
             | Limited { remaining } -> remaining
           in
           (ctxt, VNat steps)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.self" ;
      prim_id = 124 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> taddress) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.self
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] -> (ctxt, VAddress ctxt.source)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.source" ;
      prim_id = 125 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> taddress) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.source
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] -> (ctxt, VAddress ctxt.self)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.sender" ;
      prim_id = 126 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> taddress) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.sender
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] -> (ctxt, VAddress ctxt.source)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.cycle" ;
      prim_id = 127 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> tnat) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.ccycle
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] ->
           let level = Level.current ctxt.actxt in
           (ctxt, VNat (Z.of_int32 (Cycle.to_int32 level.cycle)))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Current.level" ;
      prim_id = 128 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tunit @=> tnat) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.level
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VUnit] ->
           let level = Level.current ctxt.actxt in
           (ctxt, VNat (Z.of_int32 (Raw_level.to_int32 level.level)))
       | _ -> bad_arguments p args
    ) ;

  (* Conversions *)

  register
    {
      prim_name = "Address.of_keyhash" ;
      prim_id = 129 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tkeyhash @=> taddress) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.address_of_keyhash
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VKeyHash kh] -> (ctxt, VAddress (Contract.implicit_contract kh))
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Keyhash.of_address" ;
      prim_id = 130 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> taddress @=> toption tkeyhash) ;
      prim_arity = 1 ;
    }
    Love_gas.Cost_of.keyhash_of_address
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VAddress (Implicit a)] -> (ctxt, Value.some (VKeyHash a))
       | [VAddress (Originated _)] -> (ctxt, Value.none ())
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Int.of_nat" ;
      prim_id = 131 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tnat @=> tint) ;
      prim_arity = 1 ;
    }
    gas_1
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
       | [VNat i] -> (ctxt, VInt i)
       | _ -> bad_arguments p args
    ) ;

  register
    {
      prim_name = "Nat.of_int" ;
      prim_id = 132 ;
      prim_kind = PrimFunction ;
      prim_ext_args = [] ;
      prim_type = without_args (fun () -> tint @=> toption tnat) ;
      prim_arity = 1 ;
    }
    gas_1
    (fun _apply_value ctxt _env p _xl args ->
       return @@ match args with
         [ VInt i ] ->
         if Compare.Int.(Z.compare i Z.zero < 0)
         then (ctxt, VConstr ("None", []))
         else (ctxt, VConstr ("Some", [VNat i]))
       | _ -> bad_arguments p args
    ) ;

  ()
