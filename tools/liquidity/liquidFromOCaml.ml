(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(*
let contract
      (parameter : timestamp)
      (storage: (string * timestamp * (tez * tez) *
                   ( (unit,unit) contract *
                       (unit, unit) contract *
                         (unit, unit) contract)) )
      (return : unit) =
       ...
 *)

open Asttypes
open Longident
open Parsetree
open LiquidTypes

let lo loc =
  let ppf = Format.str_formatter in
  Location.print_loc ppf loc;
  Format.flush_str_formatter ()

let ppf = Format.err_formatter

let default_args = [
    "return", Tunit;
    "storage", Tunit;
    "parameter", Tunit;
  ]

let error_loc loc msg =
  Location.print_error ppf loc;
  Printf.eprintf "Unexpected syntax %s\n%!" msg;
  raise Error

let todo_loc loc syntax =
  Location.print_error ppf loc;
  Printf.eprintf "Syntax %S not yet implemented\n%!" syntax;
  raise Error

let rec translate_type env typ =
  match typ with
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "unit" }, []) } -> Tunit
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bool" }, []) } -> Tbool
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "int" }, []) } -> Tint
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "nat" }, []) } -> Tnat
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "tez" }, []) } -> Ttez
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string" }, []) } -> Tstring
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "timestamp" }, []) } -> Ttimestamp
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key" }, []) } -> Tkey
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "signature" }, []) } -> Tsignature



  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option" }, [param_type]) } ->
     Toption (translate_type env param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "list" }, [param_type]) } ->
     Tlist (translate_type env param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "set" }, [param_type]) } ->
     Tset (translate_type env param_type)


  | { ptyp_desc = Ptyp_constr ({ txt = Lident "contract" },
                               [parameter_type; return_type]) } ->
     Tcontract (translate_type env parameter_type, translate_type env return_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "variant" },
                               [parameter_type; return_type]) } ->
     Tor (translate_type env parameter_type, translate_type env return_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "map" },
                               [parameter_type; return_type]) } ->
     Tmap (translate_type env parameter_type, translate_type env return_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "lambda" },
                               [parameter_type; return_type]) } ->
     Tlambda (translate_type env parameter_type, translate_type env return_type)

  | { ptyp_desc = Ptyp_tuple types } ->
     Ttuple (List.map (translate_type env) types)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident ty_name }, []) } ->
     begin
       try
         let ty,_ = StringMap.find ty_name env.types in
         Ttype (ty_name, ty)
       with Not_found ->
         error_loc typ.ptyp_loc "unbound type"
     end

  | { ptyp_loc } -> error_loc ptyp_loc "in type"

exception NotAConstant

(* Translate an expression, expecting a constant. Fails with [NotAConstant]
 if the expression is not a constant. *)

let rec translate_const env exp =
  match exp with
  | { pexp_desc = Pexp_construct ( { txt = Lident "()" }, None ) } ->
     CUnit, Some Tunit
  | { pexp_desc = Pexp_construct ( { txt = Lident "true" }, None ) } ->
     CBool true, Some Tbool
  | { pexp_desc = Pexp_construct ( { txt = Lident "false" }, None ) } ->
     CBool false, Some Tbool
  | { pexp_desc = Pexp_construct ( { txt = Lident "None" }, None ) } ->
     CNone, None
  | { pexp_desc = Pexp_constant (Pconst_integer (s,None)) } ->
     CInt s, Some Tint
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some 'p')) } ->
     CNat s, Some Tnat
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some 't')) } ->
     CTez s, Some Ttez
  | { pexp_desc = Pexp_constant (Pconst_float (s, Some 't')) } ->
     CTez s, Some Ttez

  | { pexp_desc = Pexp_constant (Pconst_string (s, None)) } ->
     CString s, Some Tstring

  | { pexp_desc = Pexp_tuple exps } ->
     let csts, tys = List.split (List.map (translate_const env) exps) in
     let tys =
       try
         Some (Ttuple (List.map (function
                                 | None -> raise Exit
                                 | Some ty -> ty) tys))
       with Exit -> None
     in
     CTuple csts, tys

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "[]" }, None) } ->
     CList [], None

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "::" },
                      Some { pexp_desc = Pexp_tuple [head; tail] }) } ->
     let head, head_ty = translate_const env head in
     let tail, tail_ty = translate_const env tail in
     begin
       match tail with
       | CList tail ->
          let cst = CList (head :: tail) in
          let ty =
            match head_ty, tail_ty with
            | Some head_ty, Some (Tlist tail_ty) ->
               if head_ty <> tail_ty then
                 error_loc exp.pexp_loc "inconsistent types in list";
               Some (Tlist head_ty)
            | Some head_ty, None ->
               Some (Tlist head_ty)
            | _ ->
               error_loc exp.pexp_loc "inconsistent types in list"
          in
          cst, ty
       | _ ->
          error_loc exp.pexp_loc "inconsistent types in list"
     end

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Map" }, None) } ->
     CMap [], None

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Map" }, Some pair_list) } ->
     let pair_list = translate_list pair_list in
     let pair_list = List.map translate_pair pair_list in
     let pair_list = List.map (fun (e1,e2) ->
                         let cst1, ty1 = translate_const env e1 in
                         let cst2, ty2 = translate_const env e2 in
                         (cst1, cst2), (ty1, ty2)
                       ) pair_list in
     let pair_list = List.sort compare pair_list in
     let csts, tys = List.split pair_list in
     let tys = match tys with
       | [] -> None
       | (Some ty1, Some ty2) :: tail ->

          List.iter (function
                       (Some ty1', Some ty2') when ty1' = ty1 && ty2' = ty2
                                                   -> ()
                     | _ -> error_loc exp.pexp_loc
                              "inconsistent map types"
                    )
                    tail;
          Some (Tmap (ty1, ty2))
       | _ ->
          error_loc exp.pexp_loc
            "underspecified map types"
     in
     CMap csts, tys


  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Set" }, None) } ->
     CSet [], None

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Set" }, Some pair_list) } ->
     let list = translate_list pair_list in
     let list = List.map (fun e1 ->
                    let cst1, ty1 = translate_const env e1 in
                    cst1, ty1
                  ) list in
     let list = List.sort compare list in
     let csts, tys = List.split list in
     let tys = match tys with
       | [] -> None
       | Some ty1 :: tail ->
          List.iter (function
                       Some ty1' when ty1' = ty1 -> ()
                     | _ -> error_loc exp.pexp_loc
                              "inconsistent set types"
                              )
                    tail;
          Some (Tset ty1)
       | _ ->
          error_loc exp.pexp_loc
            "underspecified set types"
     in
     CSet csts, tys


  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Some" }, Some arg) } ->
     let arg, ty = translate_const env arg in
     begin
       match ty with
       | None -> error_loc exp.pexp_loc "No type for Some arg"
       | Some ty ->
          CSome arg, Some (Toption ty)
     end

  | { pexp_desc = Pexp_constraint (cst, ty) } ->
     let cst, tyo = translate_const env cst in
     let ty = translate_type env ty in
     begin
       match tyo with
       | None ->
          (* TODO: check conformance of constant to proposed type *)
          cst, Some ty
       | Some ty_infer ->
          if ty <> ty_infer then begin
              let cst =
                match ty, cst with
                | Ttez, CString s -> CTez s
                | Tkey, CString s -> CKey s
                | Tsignature, CString s -> CSignature s
                | Tint, CNat s -> CInt s
                | _ ->
                   error_loc exp.pexp_loc  "constant type mismatch";
              in
              cst, Some ty
            end
          else
            cst, Some ty
     end

  | _ -> raise NotAConstant

and translate_list exp =
  match exp.pexp_desc with
  | Pexp_construct({ txt = Lident "[]" }, None) -> []

  | Pexp_construct({ txt = Lident "::" },
                   Some { pexp_desc = Pexp_tuple [e1; e2] }) ->
     e1 :: translate_list e2

  | _ -> error_loc exp.pexp_loc "list expected"

and translate_pair exp =
  match exp.pexp_desc with
  | Pexp_tuple [e1; e2] -> (e1, e2)
  | _ -> error_loc exp.pexp_loc "pair expected"

let mk desc = { desc; ty = (); bv = StringSet.empty; fail = false }

let rec translate_code env exp =
  let desc =
    match exp with
    | { pexp_desc = Pexp_ident ( { txt = Lident var } ) } ->
       Var (var, lo exp.pexp_loc, [])
    | { pexp_desc = Pexp_field (exp, { txt = Lident label }) } ->
       begin
         let e = translate_code env exp
         in
         match e.desc with
         | Var (name, loc, labels) ->
            Var (name, loc, labels @ [label])
         | _ -> error_loc exp.pexp_loc "variable expected"
       end
    | { pexp_desc = Pexp_setfield (exp, { txt = Lident label }, arg) } ->
       begin
         let e = translate_code env exp in
         let arg = translate_code env arg
         in
         match e.desc with
         | Var (name, loc, labels) ->
            SetVar (name, loc, labels @ [label], arg)
         | _ -> error_loc exp.pexp_loc "variable expected"
       end


    | { pexp_desc = Pexp_ident ( { txt = Ldot(Lident m, var) } ) } ->
       Var (m ^ "." ^ var, lo exp.pexp_loc, [])
    | { pexp_desc = Pexp_ifthenelse (e1, e2, None) } ->
       If (translate_code env e1,
           translate_code env e2,
           mk (Const (Tunit, CUnit)))
    | { pexp_desc = Pexp_ifthenelse (e1, e2, Some e3) } ->
       If (translate_code env e1, translate_code env e2, translate_code env e3)
    | { pexp_desc =
          Pexp_let (
              Nonrecursive,
              [
                {
                  pvb_pat =
                    {
                      ppat_desc =
                        Ppat_tuple [
                            { ppat_desc = Ppat_var { txt = result } };
                            { ppat_desc = Ppat_var { txt =
                                                       ("storage" | "_storage")as storage_name
                            } };
                          ]
                    };
                  pvb_expr =
                    { pexp_desc =
                        Pexp_apply (
                            { pexp_desc = Pexp_ident
                                            { txt =
                                                Ldot(Lident "Contract",
                                                     "call") } },
                            [
                              Nolabel, contract_exp;
                              Nolabel, tez_exp;
                              Nolabel, storage_exp;
                              Nolabel, arg_exp;
                    ]) }
                }
              ], body) } ->
       LetTransfer (storage_name, result,
                    lo exp.pexp_loc,
                    translate_code env contract_exp,
                    translate_code env tez_exp,
                    translate_code env storage_exp,
                    translate_code env arg_exp,
                    translate_code env body)

    | { pexp_desc = Pexp_let (Nonrecursive,
                              [
                                {
                                  pvb_pat = { ppat_desc =
                                                Ppat_var { txt = var } };
                                  pvb_expr = var_exp;
                                }
                              ], body) } ->
       Let (var, lo var_exp.pexp_loc,
            translate_code env var_exp, translate_code env body)

    | { pexp_desc = Pexp_sequence (exp1, exp2) } ->
       Seq (translate_code env exp1, translate_code env exp2)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                      "loop");
                                           loc } ) },
              [
                Nolabel, { pexp_desc =
                             Pexp_fun (Nolabel,None,
                                       { ppat_desc = Ppat_var { txt = name } },
                                       body
                         ) };
                Nolabel, arg
      ]) } ->
       let body = translate_code env body in
       let arg = translate_code env arg in
       Loop (name, lo exp.pexp_loc, body, arg)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident m, prim);
                                           loc } ) },
              args) } ->
       Apply(m ^ "." ^ prim, lo loc, List.map (
                                         function (Nolabel, exp) ->
                                                  translate_code env exp
                                                | (_, { pexp_loc }) ->
                                                   error_loc pexp_loc "in arg"
                                       ) args)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Lident prim; loc } ) },
              args) } ->
       Apply(prim, lo loc, List.map (
                               function (Nolabel, exp) ->
                                        translate_code env exp
                                      | (_, { pexp_loc }) ->
                                         error_loc pexp_loc "in arg"
                             ) args)

    | { pexp_desc = Pexp_match (e, cases); pexp_loc } ->
       let e = translate_code env e in
       let cases = List.map (translate_case env) cases in
       begin
         match List.sort compare cases with
         | [ "None", [], ifnone;
             "Some", [arg], ifsome] ->
            MatchOption(e, lo pexp_loc, ifnone, arg, ifsome)
         | [ "::", [head; tail], ifcons;
             "[]", [], ifnil ] ->
            MatchList(e, lo pexp_loc, head, tail, ifcons, ifnil)
         | args ->
            MatchVariant(e, lo pexp_loc, args)
       end

    | { pexp_desc =
          Pexp_fun (
              Nolabel, None,
              { ppat_desc =
                  Ppat_constraint(
                      { ppat_desc =
                          Ppat_var { txt = arg_name } },
                      arg_type)
              },
              body_exp) } ->
       let body_exp = translate_code env body_exp in
       let arg_type = translate_type env arg_type in
       Lambda (arg_name, arg_type, lo exp.pexp_loc, body_exp,
               Tunit) (* not yet inferred *)

    | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
       let lab_x_exp_list =
         List.map (function
                     ({ txt = Lident label; loc }, exp) ->
                     label, translate_code env exp
                   | ( { loc }, _) ->
                      error_loc loc "label expected"
                  ) lab_x_exp_list in
       Record (lo exp.pexp_loc, lab_x_exp_list)

    | exp ->
       match translate_const env exp with
       | _, None -> error_loc exp.pexp_loc "constant needs a type"
       | cst, Some ty -> Const (ty, cst)
       | exception NotAConstant ->
          match exp with
          | { pexp_desc = Pexp_construct (
                              { txt = Lident "Some" }, Some args) } ->
             Apply("Some", lo exp.pexp_loc, [translate_code env args])

          (* Ok, this is a very special case. We accept
not knowing the full type of the empty list because it will be infered
from the head element. We use unit for that type. *)
          | { pexp_desc =
                Pexp_construct (
                    { txt = Lident "::" },
                    Some { pexp_desc =
                             Pexp_tuple
                               [a;
                                { pexp_desc =
                                    Pexp_construct(
                                        { txt = Lident "[]" }, None) }]}) }
            ->
             Apply("::", lo exp.pexp_loc,
                   [translate_code env a;
                    mk ( Const (Tunit, CUnit))
                  ])

          | { pexp_desc = Pexp_construct (
                              { txt = Lident "::" },
                              Some { pexp_desc = Pexp_tuple [a;b]}) } ->
             Apply("::", lo exp.pexp_loc,
                   [translate_code env a;
                    translate_code env b])




          | { pexp_desc = Pexp_construct (
                              { txt = Lident lid }, args) } ->
             begin
               try
                 let (_ty_name, _ty) = StringMap.find lid env.constrs in
                 Constructor( lo exp.pexp_loc, Constr lid,
                              match args with
                              | None -> mk (Const (Tunit, CUnit))
                              | Some arg -> translate_code env arg )
               with Not_found ->
                 error_loc exp.pexp_loc "unknown constructor"
             end

          | { pexp_desc =
                Pexp_constraint (
                    { pexp_desc =
                        Pexp_construct (
                            { txt = Lident "Left" }, args) },
                    { ptyp_desc = Ptyp_constr (
                                      { txt = Lident "variant" },
                                      [ left_ty; right_ty ]
            )}) } ->
             Constructor( lo exp.pexp_loc,
                          Left (translate_type env right_ty),
                          match args with
                          | None -> mk (Const (Tunit, CUnit))
                          | Some arg -> translate_code env arg )

          | { pexp_desc =
                Pexp_constraint (
                    { pexp_desc =
                        Pexp_construct (
                            { txt = Lident "Right" }, args) },
                    { ptyp_desc = Ptyp_constr (
                                      { txt = Lident "variant" },
                                      [ left_ty; right_ty ]
            )}) } ->
             Constructor( lo exp.pexp_loc,
                          Right (translate_type env left_ty),
                          match args with
                          | None -> mk (Const (Tunit, CUnit))
                          | Some arg -> translate_code env arg )


          | { pexp_desc =
                Pexp_constraint (
                    { pexp_desc =
                        Pexp_construct (
                            { txt = Lident "Source" }, None) },
                    { ptyp_desc = Ptyp_constr (
                                      { txt = Lident "contract" },
                                      [ from_ty; to_ty ]
            )}) } ->
             Constructor( lo exp.pexp_loc,
                          Source (translate_type env from_ty,
                                  translate_type env to_ty
                                 ), mk (Const (Tunit, CUnit)) )

          (* TODO *)
          | { pexp_desc = Pexp_tuple exps } ->
             let exps = List.map (translate_code env) exps in
             begin
               try
                 let tys, csts = List.split (
                                     List.map (
                                         function
                                         | { desc = Const (ty, cst) } -> (ty, cst)
                                         | _ -> raise Exit
                                       ) exps)
                 in
                 Const (Ttuple tys, CTuple csts)
               with Exit ->
                 Apply("tuple", lo exp.pexp_loc, exps)
             end

          | { pexp_loc } ->
             error_loc pexp_loc
                       ("in expression " ^ Pprintast.string_of_expression exp)


  (*
    | { pexp_desc = Pexp_construct ({ txt = Lident id }, None) } ->
       todo_loc ("constructor " ^ id) exp.pexp_loc
   *)
  in
  mk desc

and translate_case env case =
  match case.pc_guard with
  | Some e  ->
     error_loc e.pexp_loc "when forbidden"
  | None ->
     let e = case.pc_rhs in
     let e = translate_code env e in
     match case.pc_lhs with
     | { ppat_desc = Ppat_construct ( { txt = Lident name } , None) }  ->
        (name, [], e)
     | { ppat_desc = Ppat_construct (
                         { txt = Lident name } ,
                         Some { ppat_desc = Ppat_var { txt } } ) }  ->
        (name, [txt], e)
     | { ppat_desc =
           Ppat_construct (
               { txt = Lident name } ,
               Some { ppat_desc =
                        Ppat_tuple [
                            { ppat_desc = Ppat_var { txt = var1 } };
                            { ppat_desc = Ppat_var { txt = var2 } };
                          ]
                    }
       ) }  ->
        (name, [var1; var2], e)
     | { ppat_loc } ->
        error_loc ppat_loc "bad pattern"

let rec translate_head env head_exp args =
  match head_exp with
  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_constraint(
                    { ppat_desc =
                        Ppat_var { txt =
                                     (   "parameter"
                                       | "storage"
                                       | "return"
                                     ) as arg} },
                    arg_type)
            },
            head_exp) } ->
     translate_head env head_exp
                    ((arg, translate_type env arg_type) :: args)
  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_constraint(
                    { ppat_desc =
                        Ppat_var { txt } },
                    arg_type)
            },
            head_exp);
      pexp_loc } ->
     error_loc pexp_loc (Printf.sprintf  "unexpected argument %S" txt)
  | exp ->
     let code = translate_code env exp in
     {
       code;
       parameter = List.assoc "parameter" args;
       storage = List.assoc "storage" args;
       return = List.assoc "return" args;
     }

let translate_record ty_name labels env =
  let map = ref StringMap.empty in
  let tys = List.mapi
             (fun i pld ->
               let label = pld.pld_name.txt in
               if StringMap.mem label env.labels then
                 error_loc pld.pld_loc "label already defined";

               let ty = translate_type env pld.pld_type in
               env.labels <- StringMap.add label (ty_name, i, ty) env.labels;
               map := StringMap.add label i !map;
               ty) labels
  in
  let ty = Ttuple tys in
  env.types <- StringMap.add ty_name (ty, Type_record (tys,!map)) env.types

let translate_variant ty_name constrs env =

  let constrs = List.map
             (fun pcd ->
               let constr = pcd.pcd_name.txt in
               if StringMap.mem constr env.constrs then
                 error_loc pcd.pcd_loc "constructor already defined";

               let ty = match pcd.pcd_args with
                 | Pcstr_tuple [ ty ] -> translate_type env ty
                 | Pcstr_tuple [] -> Tunit
                 | Pcstr_tuple tys -> Ttuple (List.map (translate_type env) tys)
                 | Pcstr_record _ -> error_loc pcd.pcd_loc "syntax error"
               in
               env.constrs <- StringMap.add constr (ty_name, ty) env.constrs;
               constr, ty) constrs
  in

  let rec iter constrs =
    match constrs with
    | [] -> assert false
    | [ constr, ty ] -> ty, [constr, ty, ty, ty]
    | (constr, left_ty) :: constrs ->
       let right_ty, right_constrs = iter constrs in
       let ty = Tor (left_ty, right_ty) in
       ty, (constr, ty, left_ty, right_ty) :: right_constrs
  in
  let ty, constrs = iter constrs in
  env.types <- StringMap.add ty_name (ty, Type_variant constrs) env.types

let check_version = function
  | { pexp_desc = Pexp_constant (Pconst_float (s, None)); pexp_loc } ->
    let req_version = float_of_string s in
    let liq_version = float_of_string Version.version in
    if req_version <> liq_version then
      error_loc pexp_loc ("version mismatch (requires " ^ s ^
                          " while compiler is " ^ Version.version ^ ")");
  | { pexp_loc } -> error_loc pexp_loc "version must be a floating point number"

let rec translate_structure env ast =
  match ast with
  | { pstr_desc =
         Pstr_value (
             Nonrecursive,
             [ {
                 pvb_pat = { ppat_desc = Ppat_var { txt = "version" } };
                 pvb_expr = exp;
               }
    ]) } :: ast ->
    check_version exp;
    translate_structure env ast

  | [{ pstr_desc =
         Pstr_value (
             Nonrecursive,
             [ {
                 pvb_pat = { ppat_desc = Ppat_var { txt = "contract" } };
                 pvb_expr = head_exp;
               }
    ]) } ] ->
     translate_head env head_exp default_args

  | { pstr_desc = Pstr_type (Recursive,
                             [
                               { ptype_name = { txt = ty_name };
                                 ptype_params = [];
                                 ptype_cstrs = [];
                                 ptype_private = Public;
                                 ptype_manifest = None;
                                 ptype_attributes = [];
                                 ptype_loc;
                                 ptype_kind; (* Ptype_record labels;*)
                               }
    ]) } :: ast ->
     if StringMap.mem ty_name env.types then
       error_loc ptype_loc "type already defined";
     begin match ptype_kind with
     | Ptype_record labels ->
        if List.length labels < 2 then begin
            error_loc ptype_loc "record must have at least two fields"
          end;
        translate_record ty_name labels env;
     | Ptype_abstract
       | Ptype_open -> error_loc ptype_loc "bad type definition"
     | Ptype_variant constrs ->
        if List.length constrs < 2 then begin
            error_loc ptype_loc "variant type must have at least two constructors"
          end;
        translate_variant ty_name constrs env;
     end;
     translate_structure env ast

  | [] -> Printf.eprintf "Empty file %S\n%!" env.filename; raise Error
  | { pstr_loc = loc } :: _ -> error_loc loc "at toplevel"

let predefined_constructors =
  List.fold_left (fun acc (constr, info) ->
      StringMap.add constr info acc) StringMap.empty
                 (* These constructors are never looked-up in this
                 map, hence we can provide wrong info. However, they
                 need to be there to prevent the user from overriding
                 them. *)
                 [
                   "Some", ("'a option", Tunit);
                   "None", ("'a option", Tunit);
                   "::", ("'a list", Tunit);
                   "[]", ("'a list", Tunit);
                   "Left", ("'a variant", Tunit);
                   "Right", ("'a variant", Tunit);
                   "Source", ("'a contract", Tunit);
                 ]

let predefined_types =
  List.fold_left (fun acc (constr, info) ->
      StringMap.add constr info acc) StringMap.empty
                 (* Enter predefined types with dummy-info to prevent
 the user from overriding them *)
                 [
                   "int", (Tunit, Type_variant []);
                   "unit", (Tunit, Type_variant []);
                   "bool", (Tunit, Type_variant []);
                   "nat", (Tunit, Type_variant []);
                   "tez", (Tunit, Type_variant []);
                   "string", (Tunit, Type_variant []);
                   "key", (Tunit, Type_variant []);
                   "signature", (Tunit, Type_variant []);
                   "option", (Tunit, Type_variant []);
                   "list", (Tunit, Type_variant []);
                   "map", (Tunit, Type_variant []);
                   "set", (Tunit, Type_variant []);
                 ]

let initial_env filename =
  {
    types = predefined_types;
    labels = StringMap.empty;
    constrs = predefined_constructors;
    filename;
    vars = StringMap.empty;
  }

let translate filename ast =
  let env = initial_env filename in
  translate_structure env ast, env

let read_file filename =
  try
    Pparse.parse_implementation ppf "liquidity" filename
  with x ->
       match Location.error_of_exn x with
       | Some err ->
          Format.fprintf Format.err_formatter "@[%a@]@."
                         Location.report_error err;
          raise Error
       | None -> raise x
