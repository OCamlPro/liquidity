(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let clean_ast =
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
       mapper.structure_item mapper entry
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
  let open Ast_mapper in
  LiquidOCamlPparse.ImplementationHooks.add_hook
    "liquid" (fun hook_info ast ->
      clean_ast.structure clean_ast ast
      )
