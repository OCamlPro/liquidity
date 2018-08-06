(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let translate_entry env syntax_ast mapper ast =
  let open Asttypes in
  let open Parsetree in
  let open Ast_helper in
  let open Ast_mapper in
  match ast with
  | { pstr_desc =
        Pstr_value (
          Nonrecursive,
          [ {
            pvb_pat = ({ ppat_desc = Ppat_var { txt = "main" } } as patmain);
            pvb_loc = loc_main;
            pvb_expr =
              { pexp_desc =
                  Pexp_fun (Nolabel, None,
                            ({ ppat_desc =
                                Ppat_constraint(
                                  { ppat_desc = Ppat_var { txt = "parameter" }},
                                  parameter_ty)} as cparam),
                            { pexp_desc =
                            Pexp_fun (Nolabel, None,
                                      ({ ppat_desc =
                                          Ppat_constraint(
                                              { ppat_desc = Ppat_var { txt = "storage" }},
                                              storage_ty)}  as cstor),
                                      _) }) };
      } ]) } ->
    let typed_ast = LiquidCheck.typecheck_contract
        ~warnings:true env syntax_ast in
    assert false (* TODO *)
    (* let ast = LiquidToOCaml.convert_code ~abbrev:false typed_ast.code in
     * Str.value Nonrecursive
     *   [Vb.mk patmain
     *      (Exp.fun_ Nolabel None cparam
     *         (Exp.fun_ Nolabel None cstor
     *            (mapper.expr mapper ast)
     *               ))] *)
  | _ -> assert false

let rec translate_init env syntax_ast mapper item =
  let open Asttypes in
  let open Longident in
  let open Parsetree in
  let open Ast_mapper in
  let open Ast_helper in
  let args = ref [] in
  let init_mapper = {
    mapper with
    expr = (fun imapper exp ->
        match exp with
        | { pexp_desc =
              Pexp_fun (
                Nolabel, None,
                ({ ppat_desc =
                     Ppat_constraint ({ ppat_desc = Ppat_var { txt }}, ty)
                 } as arg),
                exp) } ->
          args := (arg, txt, ty) :: !args;
          Exp.fun_ Nolabel None arg (imapper.expr imapper exp)
        | _ ->
          assert false (* TODO *)
          (* let tenv = List.fold_left (fun tenv (_, name, ty) ->
           *     fst (LiquidTypes.new_binding tenv name
           *            (LiquidFromOCaml.translate_type env ty))
           *   ) (LiquidTypes.empty_typecheck_env ~warnings:true
           *        LiquidTypes.dummy_contract_sig env) !args
           * in
           * let sy_init = LiquidFromOCaml.translate_expression env exp in
           * let ty_init = LiquidCheck.typecheck_code tenv sy_init in
           * let init_ast = LiquidToOCaml.convert_code ~abbrev:false ty_init in
           * mapper.expr mapper init_ast *)
      )
  } in
  init_mapper.structure_item init_mapper item

let clean_ast env syntax_ast =
  let open Asttypes in
  let open Longident in
  let open Parsetree in
  let open Ast_mapper in
  let open Ast_helper in
  let lident ~loc s = { txt = Longident.parse s; loc } in
  let exp_ident ~loc s =
    Exp.ident ~loc (lident ~loc s) in
  let exp_unit ~loc = Exp.construct ~loc { txt = Lident "()"; loc } None in
  let exp_string ~loc s = Exp.constant ~loc (Pconst_string (s,None)) in
  {
    default_mapper with
    structure_item = (fun mapper item ->
    match item with
    | { pstr_desc =
          Pstr_extension
            (({ Asttypes.txt = "version" },
              PStr [{ pstr_desc = Pstr_eval (exp,[])}]),[])
      } ->
      Str.eval (Exp.constant (Const.int 0))

    | { pstr_desc =
          Pstr_extension
            (({ Asttypes.txt = "entry" },
              PStr [entry]),[])
      } ->
      translate_entry env syntax_ast mapper entry

    | { pstr_desc =
          Pstr_extension
            (({ Asttypes.txt = "init" },
              PStr [init]),[])
      } ->
      translate_init env syntax_ast mapper init

    | _ ->
       default_mapper.structure_item mapper item
  );
    expr = (fun mapper expr ->
      let loc = expr.pexp_loc in
      match expr.pexp_desc with
      | Pexp_constant (
                  Pconst_integer (s, Some 'p')
                | Pconst_integer (s, None)
              )
         ->
           Exp.apply ~loc (exp_ident ~loc "Int.of_string")
                     [Nolabel, exp_string ~loc s]

      | Pexp_constant (Pconst_integer (s, Some '\231'))
        ->
         Exp.apply ~loc (exp_ident ~loc "Tez.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constant (Pconst_integer (s, Some '\232'))
        ->
         Exp.apply ~loc (exp_ident ~loc "Timestamp.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constraint (
          { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\233')) },
          { ptyp_desc = Ptyp_constr ({ txt = Lident "address" }, [])}
        )
        ->
         Exp.apply ~loc (exp_ident ~loc "Address.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constraint (
          { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\233')) },
          { ptyp_desc = Ptyp_constr ({ txt = Lident "contract" }, [_])}
        )
        ->
         Exp.apply ~loc (exp_ident ~loc "Contract.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constant (Pconst_integer (s, Some '\233'))
        ->
         Exp.apply ~loc (exp_ident ~loc "Key_hash.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constant (Pconst_integer (s, Some '\234'))
        ->
         Exp.apply ~loc (exp_ident ~loc "Key.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constant (Pconst_integer (s, Some '\235'))
        ->
         Exp.apply ~loc (exp_ident ~loc "Signature.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constant (Pconst_integer (s, Some '\236'))
        ->
         Exp.apply ~loc (exp_ident ~loc "Address.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_constant (Pconst_integer (s, Some '\237'))
        ->
         exp_string ~loc s

      | Pexp_constant (
                  Pconst_float (s, Some '\231')
              )
        ->
         Exp.apply ~loc (exp_ident ~loc "Tez.of_string")
                   [Nolabel, exp_string ~loc s]

      | Pexp_construct ({ txt = Lident "Map" }, None)
        ->
         Exp.apply ~loc (exp_ident ~loc "Map.empty") [Nolabel, exp_unit ~loc]

      | Pexp_construct (
              { txt = Lident "Map" }, Some list
              )
        ->
         let list = default_mapper.expr mapper list in
         Exp.apply ~loc (exp_ident ~loc "Map.make") [Nolabel, list]


      | Pexp_construct ({ txt = Lident "BigMap" }, None)
        ->
         Exp.apply ~loc (exp_ident ~loc "BigMap.empty") [Nolabel, exp_unit ~loc]

      | Pexp_construct (
              { txt = Lident "BigMap" }, Some list
              )
        ->
         let list = default_mapper.expr mapper list in
         Exp.apply ~loc (exp_ident ~loc "BigMap.make") [Nolabel, list]

      | Pexp_construct (
              { txt = Lident "Set" }, None
              )
        ->
         Exp.apply ~loc (exp_ident ~loc "Set.empty") [Nolabel, exp_unit ~loc]

      | Pexp_construct (
              { txt = Lident "Set" }, Some list
              )
        ->
         let list = default_mapper.expr mapper list in
         Exp.apply ~loc (exp_ident ~loc "Set.make") [Nolabel, list]


      | Pexp_construct (
              { txt = Lident "Source" }, None
              )
        ->
         Exp.apply ~loc (exp_ident ~loc "Contract.source")
                   [Nolabel, exp_unit ~loc]

      | Pexp_setfield (record, label1, value) ->
         let value = default_mapper.expr mapper value in
         begin
           match record.pexp_desc with
           | Pexp_field (record, label2) ->
              let record = default_mapper.expr mapper record in
              let id = "record#" in
              Exp.let_
                ~loc Nonrecursive
                [Vb.mk ~loc (Pat.var ~loc { txt = id; loc }) record]
                (Exp.record
                   ~loc
                   [label2,
                    Exp.record
                      ~loc [
                        label1, value
                      ]
                      (Some (Exp.field ~loc (Exp.ident ~loc
                                                       { txt = Lident id; loc})
                                       label2))
                   ]
                   (Some (Exp.ident ~loc { txt = Lident id; loc}))
                )
           | _ ->
              let record = default_mapper.expr mapper record in
              Exp.record ~loc [label1, value] (Some record)
         end

      | Pexp_extension (
          { txt = "nat" },
          PStr [{ pstr_desc = Pstr_eval (
              { pexp_desc = Pexp_match (e, cases); pexp_loc=loc },
              [])
            }]
        ) ->
        let exception Found of (string * Parsetree.expression) in
        let find_constr c =
          try
            List.iter (fun case ->
                match case.pc_lhs with
                | { ppat_desc = Ppat_construct (
                    { txt = Lident name } ,
                    Some { ppat_desc = Ppat_var { txt = var } }) }
                  when name = c ->
                  raise (Found (var, default_mapper.expr mapper case.pc_rhs))
                | { ppat_desc = Ppat_construct (
                    { txt = Lident name } ,
                    Some { ppat_desc = Ppat_any }) }
                  when name = c ->
                  raise (Found ("_", default_mapper.expr mapper case.pc_rhs))
                | _ -> ()
              ) cases;
            Format.eprintf "No constructor %s known@." c;
            assert false
          with Found v_e -> v_e
        in
        let id = "matchnat#" in
        let p, ifplus = find_constr "Plus" in
        let m, ifminus = find_constr "Minus" in
        let exp = default_mapper.expr mapper e in
        Exp.let_ ~loc Nonrecursive
          [ Vb.mk (Pat.var { txt = id; loc }) exp]
          (Exp.ifthenelse ~loc
             (Exp.apply ~loc (exp_ident ~loc ">=")
                [ Nolabel, exp_ident ~loc id ;
                  Nolabel, Exp.apply ~loc (exp_ident ~loc "Int.of_string")
                    [Nolabel, exp_string ~loc "0"]])
             (Exp.let_ ~loc Nonrecursive
                [ Vb.mk (Pat.var { txt = p; loc })
                    (Exp.apply ~loc:ifplus.pexp_loc
                       (exp_ident ~loc "abs")
                       [Nolabel, exp_ident ~loc id])]
                ifplus)
             (Some (Exp.let_ ~loc Nonrecursive
                      [ Vb.mk (Pat.var { txt = m; loc })
                          (Exp.apply ~loc:ifminus.pexp_loc
                             (exp_ident ~loc "abs")
                             [Nolabel, exp_ident ~loc id])]
                      ifminus)))

      | _ -> default_mapper.expr mapper expr
    );
    (* `String of int * string` is compiled as `String of (int * string)` *)
    constructor_declaration = (fun mapper pcd ->
      match pcd.pcd_args with
      | Pcstr_tuple []
      | Pcstr_tuple [_] -> pcd
      | Pcstr_tuple list -> { pcd with pcd_args =
                                         Pcstr_tuple [Typ.tuple list] }
      | _ -> pcd
    );
  }


let init () =
  let open Asttypes in
  let open Longident in
  let open Parsetree in
  let open Ast_mapper in
  let open Ast_helper in
  LiquidOCamlPparse.ImplementationHooks.add_hook
    "liquid" (fun hook_info ast ->
          try

            let syntax_ast, _, env =
              LiquidFromOCaml.translate ~filename:"<<>>" ast in
            let clean_mapper = clean_ast env syntax_ast in
            clean_mapper.structure clean_mapper ast

          with
          | LiquidTypes.LiquidError error ->
            LiquidLoc.report_error Format.err_formatter error;
            exit 1

      )
