(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* We use '\231' as suffix for integers and floats to encode the "tz"
    suffix.  *)

open LiquidTypes
open LiquidOCamlParser
open LiquidInfer

type 'a ast_elt =
  | Syn_value of string * inline * 'a
  | Syn_contract of 'a contract
  | Syn_entry of 'a entry
  | Syn_init of 'a init

(* redefined keywords of the modified OCaml lexer *)
let liquidity_keywords = [
  "and", AND;
  "as", AS;
  "assert", ASSERT;
  "begin", BEGIN;
  (*    "class", CLASS; *)
  (*    "constraint", CONSTRAINT; *)
  "do", DO;
  "done", DONE;
  "downto", DOWNTO;
  "else", ELSE;
  "end", END;
  (* "exception", EXCEPTION; *)
  "external", EXTERNAL;
  "false", FALSE;
  "for", FOR;
  "fun", FUN;
  "function", FUNCTION;
  (* "functor", FUNCTOR; *)
  "if", IF;
  "in", IN;
  (* "include", INCLUDE; *)
  (* "inherit", INHERIT; *)
  (* "initializer", INITIALIZER; *)
  (* "lazy", LAZY; *)
  "let", LET;
  "match", MATCH;
  (* "entry", METHOD; *)
  "contract", MODULE;
  (* "mutable", MUTABLE; *)
  (* "new", NEW; *)
  (* "nonrec", NONREC; *)
  (* "object", OBJECT; *)
  "of", OF;
  (* "open", OPEN; *)
  "or", OR;
  (*  "parser", PARSER; *)
  (* "private", PRIVATE; *)
  "rec", REC;
  "sig", SIG;
  "struct", STRUCT;
  "then", THEN;
  "to", TO;
  "true", TRUE;
  (* "try", TRY; *)
  "type", TYPE;
  "val", VAL;
  (* "virtual", VIRTUAL; *)

  (* "when", WHEN; *)
  "while", WHILE;
  "with", WITH;

  "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
  "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
  "mod", INFIXOP3("mod");
  "land", INFIXOP3("land");
  "lsl", INFIXOP4("lsl");
  "lsr", INFIXOP4("lsr");
  "xor", INFIXOP3("xor"); (* Should be INFIXOP2 *)
  "asr", INFIXOP4("asr")
]

let () =
  LiquidOCamlLexer.define_keywords liquidity_keywords

let ident_counter = ref 0

(* The minimal version of liquidity files that are accepted by this compiler *)
let minimal_version = 0.4

(* The maximal version of liquidity files that are accepted by this compiler *)
let maximal_version = 0.54


open Asttypes
open Longident
open Parsetree
open LiquidTypes

let str_of_id id = String.concat "." (Longident.flatten id)

let loc_of_loc = LiquidLoc.loc_of_location


let ppf = Format.err_formatter

let mk_inner_env env contractname =
  {
    types = StringMap.empty;
    contract_types = StringMap.empty;
    labels = StringMap.empty;
    constrs = StringMap.empty;
    ext_prims = StringMap.empty;
    filename = env.filename;
    top_env = Some env;
    contractname;
  }

let rec fold_env_proj f acc env proj =
  let acc = StringMap.fold f (proj env) acc in
  match env.top_env with
  | None -> acc
  | Some env -> fold_env_proj f acc env proj

let rec shadow name env =
  let prefix = name ^ "." in
  let erase_in env proj =
    fold_env_proj (fun s t acc ->
        if LiquidMisc.has_prefix ~prefix s then
          StringMap.remove s acc
        else acc
      ) (proj env) env proj in
  env.types <- erase_in env (fun env -> env.types);
  env.contract_types <- erase_in env (fun env -> env.contract_types);
  env.labels <- erase_in env (fun env -> env.labels);
  env.constrs <- erase_in env (fun env -> env.constrs);
  ()

let lift_env rename = function
  | { top_env = None } -> assert false
  | { types; contract_types; labels; constrs; ext_prims;
      filename; contractname = inner_name;
      top_env = Some top_env } as env ->
    let lift_name n = rename inner_name n in
    let rec lift_type ty = match ty with
      | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp
      | Tkey | Tkey_hash | Tsignature | Toperation | Taddress | Tfail -> ty
      | Ttuple l -> Ttuple (List.map lift_type l)
      | Toption t -> Toption (lift_type t)
      | Tlist t -> Tlist (lift_type t)
      | Tset t -> Tset (lift_type t)
      | Tmap (t1, t2) -> Tmap (lift_type t1, lift_type t2)
      | Tbigmap (t1, t2) -> Tbigmap (lift_type t1, lift_type t2)
      | Tor (t1, t2) -> Tor (lift_type t1, lift_type t2)
      | Tlambda (t1, t2) -> Tlambda (lift_type t1, lift_type t2)
      | Tcontract c_sig -> Tcontract (lift_contract_sig c_sig)
      | Trecord (name, fields) when StringMap.mem name env.types ->
        Trecord (lift_name name,
                 List.map (fun (f, ty) -> lift_name f, lift_type ty) fields)
      | Trecord (name, fields) ->
        Trecord (name, List.map (fun (f, ty) -> f, lift_type ty) fields)
      | Tsum (name, constrs) when StringMap.mem name env.types ->
        Tsum (lift_name name,
              List.map (fun (c, ty) -> lift_name c, lift_type ty) constrs)
      | Tsum (name, constrs) ->
        Tsum (name, List.map (fun (c, ty) -> c, lift_type ty) constrs)
      | Tclosure ((t1, t2), t3) ->
        Tclosure ((lift_type t1, lift_type t2), lift_type t3)
      | Tvar tvr ->
        let tv = Ref.get tvr in
        begin match tv.tyo with
          | None -> ty
          | Some ty -> Ref.set tvr { tv with tyo = Some (lift_type ty) }; ty
        end
      | Tpartial _ -> raise (Invalid_argument "lift_type")
    and lift_contract_sig c_sig =
      { sig_name = c_sig.sig_name;
        entries_sig = List.map (fun es ->
            { es with parameter = lift_type es.parameter }
          ) c_sig.entries_sig
      }
    in
    let lift_to_top map top lift_v =
      StringMap.fold (fun s v top ->
          StringMap.add (lift_name s) (lift_v v) top
        ) map top in
    (* shadow previous definitions of inner_name *)
    shadow inner_name top_env;
    top_env.types <- lift_to_top types top_env.types
        (fun mk p -> lift_type (mk p));
    top_env.contract_types <-
      lift_to_top contract_types top_env.contract_types lift_contract_sig;
    top_env.labels <- lift_to_top labels top_env.labels
        (fun (lab, n) -> lift_name lab, n);
    top_env.constrs <- lift_to_top constrs top_env.constrs
        (fun c -> lift_name c);
    top_env.ext_prims <- lift_to_top ext_prims top_env.ext_prims
        (fun e -> { e with atys = List.map lift_type e.atys;
                           rty = lift_type e.rty });
    lift_type

let lift_inner_env =
  let lift_name inner_name n = String.concat "." [inner_name; n] in
  lift_env lift_name

let error_loc loc fmt =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc ("Unexpected syntax: "^^fmt^^"%!")

let error_version loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "version mismatch %s%!" msg

let unbound_type loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound type %S%!" ty

let unbound_contract_type loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound contract type %S%!" ty

let unbound_contract loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound contract %S%!" ty

let error_arg loc arg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unexpected argument %S%!" arg

let todo_loc loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Syntax %S not yet implemented%!" msg

let has_stack typ = match typ.ptyp_desc with
  | Ptyp_extension ( { txt = "stack" }, _ ) -> true
  | _ -> false

let remove_stack typ = match typ.ptyp_desc with
  | Ptyp_extension ( { txt = "stack" }, PTyp  t ) -> t
  | _ -> typ

let rec translate_type env ?expected typ =
  if has_stack typ then
    error_loc typ.ptyp_loc "Attribute [%%stack] forbidden in this context";
  match typ with
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "unit" }, []) } -> Tunit
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bool" }, []) } -> Tbool
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "int" }, []) } -> Tint
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "nat" }, []) } -> Tnat
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "tez" }, []) } -> Ttez
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string" }, []) } -> Tstring
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bytes" }, []) } -> Tbytes
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "timestamp" }, []) } -> Ttimestamp
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key" }, []) } -> Tkey
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key_hash" }, []) } -> Tkey_hash
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "signature" }, []) } -> Tsignature
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "operation" }, []) } -> Toperation
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "address" }, []) } -> Taddress

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option" }, [param_type]) } ->
    let expected = match expected with
      | Some (Toption ty) -> Some ty
      | _ -> None
    in
    Toption (translate_type env ?expected param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "list" }, [param_type]) } ->
    let expected = match expected with
      | Some (Tlist ty) -> Some ty
      | _ -> None
    in
    Tlist (translate_type env ?expected param_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "set" }, [param_type]);
      ptyp_loc } ->
    let expected = match expected with
      | Some (Tset ty) -> Some ty
      | _ -> None
    in
    let elt_type = translate_type env ?expected param_type in
    if not @@ comparable_type elt_type then
      error_loc ptyp_loc
        "type %S is not comparable, it cannot be used as element of a set"
        (LiquidPrinter.Liquid.string_of_type elt_type);
    Tset elt_type

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "variant" },
                               [left_type; right_type]) } ->
    let expected1, expected2 = match expected with
      | Some (Tor (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    Tor (translate_type env ?expected:expected1 left_type,
         translate_type env ?expected:expected2 right_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "map" },
                               [key_type; val_type]);
      ptyp_loc } ->
    let expected1, expected2 = match expected with
      | Some (Tmap (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    let key_type = translate_type env ?expected:expected1 key_type in
    if not @@ comparable_type key_type then
      error_loc ptyp_loc
        "type %S is not comparable, it cannot be used as key in a map"
        (LiquidPrinter.Liquid.string_of_type key_type);
    let val_type = translate_type env ?expected:expected2 val_type in
    Tmap (key_type, val_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "big_map" },
                               [key_type; val_type]);
      ptyp_loc } ->
    let expected1, expected2 = match expected with
      | Some (Tbigmap (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    let key_type = translate_type env ?expected:expected1 key_type in
    if not @@ comparable_type key_type then
      error_loc ptyp_loc
        "type %S is not comparable, it cannot be used as key in a big map"
        (LiquidPrinter.Liquid.string_of_type key_type);
    let val_type = translate_type env ?expected:expected2 val_type in
    Tbigmap (key_type, val_type)

  | { ptyp_desc = Ptyp_arrow (_, parameter_type, return_type) } ->
    (* TODO ? *)
    (* let expected = match expected with
     *   | Some (Tcontract ty) -> Some ty
     *   | _ -> None
     * in *)
    Tlambda (translate_type env (* ?expected:expected *) parameter_type,
             translate_type env return_type)

  | { ptyp_desc = Ptyp_tuple [ty] } ->
    translate_type env ?expected ty

  | { ptyp_desc = Ptyp_tuple types } ->
    let expecteds = match expected with
      | Some (Ttuple tys) when List.length types = List.length tys ->
        List.map (fun ty -> Some ty) tys
      | _ -> List.map (fun _ -> None) types
    in
    Ttuple (List.map2 (fun ty expected -> translate_type env ?expected ty)
              types expecteds)

  | { ptyp_desc = Ptyp_constr ({ txt = ty_name }, []) }
    when Longident.last ty_name = "instance" ->
    let contract_type_name =
      match List.rev (Longident.flatten ty_name) with
      | _ :: rpath -> String.concat "." (List.rev rpath)
      | _ -> assert false
    in
    begin
      try Tcontract (find_contract_type contract_type_name env)
      with Not_found ->
        unbound_contract_type typ.ptyp_loc contract_type_name
    end

  | { ptyp_desc = Ptyp_constr ({ txt = ty_name }, params); ptyp_loc } ->
    let ty_name = str_of_id ty_name in
    begin
      try
        find_type ty_name env (List.map (translate_type env) params)
      with
      | Invalid_argument _ -> error_loc ptyp_loc "too many type arguments"
      | Not_found -> unbound_type typ.ptyp_loc ty_name
    end

  | { ptyp_desc = Ptyp_any; ptyp_loc } ->
    begin match expected with
      | Some ty -> ty
      | None -> error_loc ptyp_loc "cannot infer type"
    end

  | { ptyp_desc = Ptyp_var id; ptyp_loc } ->
    Tvar (Ref.create { id; tyo = None })

  | { ptyp_loc } -> error_loc ptyp_loc "in type"

let translate_ext_type env typ =
  let rec aux noargs tvs atys typ = match typ with
    (* Type argument *)
    | { ptyp_desc = Ptyp_arrow (_, { ptyp_desc =
                                       Ptyp_extension ( { txt = "type" }, PTyp { ptyp_desc = Ptyp_var tv })
                                   }, return_type); ptyp_loc } ->
      if atys <> [] || noargs then
        error_loc ptyp_loc "Type arguments must come first";
      if List.mem tv tvs then
        error_loc ptyp_loc "Type variable '%s already used in this primitive" tv;
      aux false (tv :: tvs) atys return_type

    (* Argument *)
    | { ptyp_desc = Ptyp_arrow (_, parameter_type, return_type); ptyp_loc } ->
      let ty = translate_type env (remove_stack parameter_type) in
      let stack = has_stack parameter_type in
      if noargs then
        error_loc ptyp_loc "This primitive does not expect any argument"
      else if stack then
        aux false tvs (ty :: atys) return_type
      else if ty = Tunit && atys = [] then
        aux true tvs [] return_type
      else
        error_loc ptyp_loc "Attribute [%%stack] expected";

      (* Result : tuple *)
    | { ptyp_desc = Ptyp_tuple types; ptyp_loc } when atys <> [] || noargs ->
      let tys = List.map (fun ty -> translate_type env (remove_stack ty)) types in
      let stack = has_stack typ in
      if List.exists (fun t -> has_stack t = stack) types then
        error_loc ptyp_loc
          "[%%stack] must be either on the whole tuple or on ALL its components";
      let rtys = if not stack then tys else [Ttuple tys] in
      List.rev tvs, List.rev atys, rtys

    (* Result : any other type *)
    | { ptyp_loc } when atys <> [] || noargs ->
      let ty = translate_type env (remove_stack typ) in
      let stack = has_stack typ in
      if ty <> Tunit && not stack then
        error_loc ptyp_loc "Attribute [%%stack] expected on return value";
      let rtys = if not stack then [] else [ty] in
      List.rev tvs, List.rev atys, rtys

    (* Non-function type *)
    | { ptyp_loc } ->
      error_loc ptyp_loc "Primitives must be functions"
  in
  aux false [] [] typ


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
    CNone, Some (Toption (fresh_tvar ()))
  | { pexp_desc = Pexp_constant (Pconst_integer (s,None)) } ->
    CInt (LiquidNumber.integer_of_liq s), Some Tint
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some 'p')) } ->
    CNat (LiquidNumber.integer_of_liq s), Some Tnat

  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\231')) } ->
    CTez (LiquidNumber.tez_of_liq s), Some Ttez
  | { pexp_desc = Pexp_constant (Pconst_float (s, Some '\231')) } ->
    CTez (LiquidNumber.tez_of_liq s), Some Ttez

  (* Timestamps *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\232')) } ->
    CTimestamp (ISO8601.of_string s), Some Ttimestamp

  (* Key_hash *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\233')) } ->
    CKey_hash s, Some Tkey_hash

  (* Address *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\236')) } ->
    CAddress s, Some Taddress

  (* Key *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\234')) } ->
    CKey s, Some Tkey

  (* Signature *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\235')) } ->
    CSignature s, Some Tsignature

  (* Bytes *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\237')) } ->
    CBytes s, Some Tbytes

  | { pexp_desc = Pexp_constant (Pconst_string (s, None)) } ->
    CString s, Some Tstring

  | { pexp_desc = Pexp_tuple [exp] } ->
    translate_const env exp

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
    CList [], Some (Tlist (fresh_tvar ()))

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
            let is_var = match tail_ty with Tvar _ -> true | _ -> false in
            if not is_var && not @@ eq_types head_ty tail_ty then
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

  | { pexp_desc = Pexp_construct ({ txt = Lident "Map" }, None) } ->
    CMap [], Some (Tmap (fresh_tvar (), fresh_tvar ()))

  | { pexp_desc = Pexp_construct ({ txt = Lident "BigMap" }, None) } ->
    CBigMap [], Some (Tbigmap (fresh_tvar (), fresh_tvar ()))

  | { pexp_desc = Pexp_construct (
      { txt = Lident ("Map" | "BigMap" as map_kind) },
      Some pair_list) } ->
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
      | (Some ty1, Some ty2) :: tail ->

        List.iter (function
              (Some ty1', Some ty2') when ty1' = ty1 && ty2' = ty2
              -> ()
            | _ -> error_loc exp.pexp_loc
                     "inconsistent map types"
          )
          tail;
        begin match map_kind with
          | "Map" -> Some (Tmap (ty1, ty2))
          | "BigMap" -> Some (Tbigmap (ty1, ty2))
          | _ -> assert false
        end
      | _ -> (*None*)
        begin match map_kind with
          | "Map" -> Some (Tmap (fresh_tvar (), fresh_tvar ()))
          | "BigMap" -> Some (Tbigmap (fresh_tvar (), fresh_tvar ()))
          | _ -> assert false
        end
    in
    begin match map_kind with
      | "Map" -> CMap csts, tys
      | "BigMap" -> CBigMap csts, tys
      | _ -> assert false
    end

  | { pexp_desc = Pexp_construct (
      { txt = Lident "Set" }, None) } ->
    CSet [], Some (Tset (fresh_tvar ()))

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
      | Some ty1 :: tail ->
        List.iter (function
              Some ty1' when ty1' = ty1 -> ()
            | _ -> error_loc exp.pexp_loc
                     "inconsistent set types"
          )
          tail;
        Some (Tset ty1)
      | _ -> None
    in
    CSet csts, tys


  | { pexp_desc = Pexp_construct (
      { txt = Lident "Some" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Toption ty)
    in
    CSome arg, ty

  | { pexp_desc = Pexp_construct (
      { txt = Lident "Left" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Tor (ty, fresh_tvar ()))
    in
    CLeft arg, ty

  | { pexp_desc = Pexp_construct (
      { txt = Lident "Right" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Tor (fresh_tvar (), ty))
    in
    CRight arg, ty

  | { pexp_desc = Pexp_construct ({ txt = lid }, args) } ->
    let lid = str_of_id lid in
    begin
      try
        let c =
          match args with
          | None -> CUnit
          | Some args ->
            let c, ty_opt = translate_const env args in
            c
        in
        let ty_name = find_constr lid env in
        let ty = find_type ty_name env [] in
        CConstr (lid, c), Some ty
      with Not_found -> raise NotAConstant
    end

  | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
    let lab_x_exp_list =
      List.map (fun ({ txt = label; loc }, exp) ->
          try
            let label = str_of_id label in
            let c, ty_opt = translate_const env exp in
            label, c
          with Not_found ->
            error_loc exp.pexp_loc "unknown label %s" (str_of_id label)
        ) lab_x_exp_list in
    let ty = match lab_x_exp_list with
      | [] -> error_loc exp.pexp_loc "empty record"
      | (label, _) :: _ ->
        try
          let ty_name, _ = find_label label env in
          find_type ty_name env []
        with Not_found -> error_loc exp.pexp_loc "unknown label %s" label
    in
    CRecord lab_x_exp_list, Some ty

  | { pexp_desc = Pexp_constraint (cst, ty) } ->
    let cst, tyo = translate_const env cst in
    let ty = translate_type env ?expected:tyo ty in
    cst, Some ty

  | _ -> raise NotAConstant

and translate_list exp =
  match exp.pexp_desc with
  | Pexp_tuple [exp] -> translate_list exp
  | Pexp_construct({ txt = Lident "[]" }, None) -> []

  | Pexp_construct({ txt = Lident "::" },
                   Some { pexp_desc = Pexp_tuple [e1; e2] }) ->
    e1 :: translate_list e2

  | _ -> error_loc exp.pexp_loc "list expected"

and translate_pair exp =
  match exp.pexp_desc with
  | Pexp_tuple [exp] -> translate_pair exp
  | Pexp_tuple [e1; e2] -> (e1, e2)
  | _ -> error_loc exp.pexp_loc "pair expected"


let mk ~loc desc = mk ~loc desc ()

let vars_info_pat env pat =
  let rec vars_info_pat_aux acc indexes = function
    | { ppat_desc = Ppat_constraint (pat, ty) } ->
      let acc, _ = vars_info_pat_aux acc indexes pat in
      acc, translate_type env ty

    | { ppat_desc = Ppat_var { txt = var; loc } } ->
      (var, loc_of_loc loc, indexes) :: acc,
      fresh_tvar ()

    | { ppat_desc = Ppat_any; ppat_loc } ->
      ("_", loc_of_loc ppat_loc, indexes) :: acc,
      fresh_tvar ()

    | { ppat_desc = Ppat_construct ({ txt = Lident "()" }, None); ppat_loc } ->
      ("_", loc_of_loc ppat_loc, indexes) :: acc,
      Tunit

    | { ppat_desc = Ppat_tuple [pat] } ->
      vars_info_pat_aux acc indexes pat

    | { ppat_desc = Ppat_tuple pats } ->
      let _, acc, tys =
        List.fold_left (fun (i, acc, tys) pat ->
            let acc, ty = vars_info_pat_aux acc (i :: indexes) pat in
            i + 1, acc, ty :: tys
          ) (0, acc, []) pats
      in
      acc, Ttuple (List.rev tys)

    | { ppat_loc } ->
      error_loc ppat_loc "cannot deconstruct this pattern"
  in
  vars_info_pat_aux [] [] pat

let access_of_deconstruct var_name loc indexes =
  let a = mk ~loc (Var var_name) in
  List.fold_right (fun i a ->
      mk ~loc (Apply {
          prim = Prim_tuple_get;
          args = [
            a;
            mk ~loc (Const { ty = Tnat;
                             const = CNat (LiquidNumber.integer_of_int i) })
          ] })
    ) indexes a

let deconstruct_pat env pat e =
  let vars_infos, ty = vars_info_pat env pat in
  match vars_infos with
  | [] -> assert false
  | [nname, nloc, []] -> { nname; nloc }, ty, e
  | _ ->
    let var_name =
      String.concat "_" ( "" :: (List.rev_map (fun (v,_,_) -> v) vars_infos)) in
    let e =
      List.fold_left (fun e (v, loc, indexes) ->
          let access = access_of_deconstruct var_name loc indexes in
          mk ~loc (Let { bnd_var = { nname = v; nloc = loc };
                         inline = InAuto;
                         bnd_val = access; body = e })
        ) e vars_infos
    in
    let nloc = match vars_infos, List.rev vars_infos with
      | (_, first_loc, _) :: _, (_, last_loc, _) :: _ ->
        LiquidLoc.merge first_loc last_loc
      | _ -> assert false
    in
    ({ nname = var_name; nloc }, ty, e)

let order_labelled_args loc labels args =
  let labelled_exps, args =
    List.fold_left (fun (acc, args) label ->
        let exps, args =
          List.partition
            (function (Labelled l, _) -> l = label | _ -> false)
            args in
        (label, exps) :: acc, args
      ) ([], args) labels in
  let labelled_exps = List.rev labelled_exps in
  let args, extra_args = List.fold_left (fun (acc, args) labelled ->
      let exp, args = match labelled, args with
        | (_, [_, exp]), _ -> exp, args (* only one match *)
        | (_, []), (Nolabel, exp) :: args -> exp, args (* no match, take first one *)
        | (_, []), (Labelled label, exp) :: args ->
          error_loc loc "unknown label %s" label
        | (label, []), [] ->
          error_loc loc "missing argument %s" label
        | (label, _), _ ->
          error_loc loc "multiple arguments labelled with %s" label in
      (exp :: acc, args)
    ) ([], args) labelled_exps in
  match extra_args with
  | [] -> List.rev args
  | _ -> error_loc loc "too many arguments"

let add_type_alias ~loc ty_name params ty env =
  let pset, params = List.fold_left (fun (acc, params) -> function
      | { ptyp_desc = Ptyp_var alpha; ptyp_loc }, Invariant ->
        if StringSet.mem alpha acc then
          error_loc ptyp_loc "Type parameter '%s occurs several times" alpha;
          if ty_name = "storage" && alpha.[0] <> '_' then
            LiquidLoc.warn (loc_of_loc ptyp_loc) (WeakParam alpha);
        StringSet.add alpha acc, alpha :: params
      | { ptyp_loc }, _ ->
        error_loc ptyp_loc "Type parameter not allowed";
    ) (StringSet.empty, []) params in
  let params = List.rev params in
  if StringMap.mem ty_name env.types then
    error_loc loc "type %s already defined" ty_name;
  let tvars = free_tvars ty in
  StringSet.iter (fun v ->
      if not @@ StringSet.mem v pset then
        error_loc loc "Unbound type parameter '%s" v) tvars;
  let mk_ty pvals =
    let subst = make_subst params pvals in
    instantiate_to subst ty
  in
  env.types <- StringMap.add ty_name mk_ty env.types

let translate_record ~loc ty_name params labels env =
  let rtys = List.mapi
      (fun i pld ->
         let label = pld.pld_name.txt in
         try
           find_label label env |> ignore;
           error_loc pld.pld_loc "label %s already defined" label;
         with Not_found ->
           let ty = translate_type env pld.pld_type in
           env.labels <- StringMap.add label (ty_name, i) env.labels;
           (label, ty)
      ) labels
  in
  let ty = Trecord (ty_name, rtys) in
  add_type_alias ~loc ty_name params ty env

let translate_variant ~loc ty_name params constrs env =
  let constrs = List.map
      (fun pcd ->
         let constr = pcd.pcd_name.txt in
         try
           find_constr constr env |> ignore;
           error_loc pcd.pcd_loc "constructor %s already defined" constr;
         with Not_found ->
           let ty = match pcd.pcd_args with
             | Pcstr_tuple [ ty ] -> translate_type env ty
             | Pcstr_tuple [] -> Tunit
             | Pcstr_tuple tys -> Ttuple (List.map (translate_type env) tys)
             | Pcstr_record _ -> error_loc pcd.pcd_loc "syntax error"
           in
           env.constrs <- StringMap.add constr ty_name env.constrs;
           (constr, ty)
      ) constrs
  in
  let ty = Tsum (ty_name, constrs) in
  add_type_alias ~loc ty_name params ty env

let check_version = function
  | { pexp_desc = Pexp_constant (Pconst_float (s, None)); pexp_loc } ->
    let req_version = float_of_string s in
    if req_version < minimal_version then
      Printf.kprintf (error_version pexp_loc)
        "(requires %F while compiler has minimal %F )"
        req_version minimal_version;
    if req_version > maximal_version then
      error_loc pexp_loc
        "(requires %F while compiler has maximal %F )"
        req_version maximal_version;
  | { pexp_loc } -> error_loc pexp_loc "version must be a floating point number"

let filter_contracts acc =
  List.fold_left (fun acc -> function
      | Syn_contract c -> StringMap.add c.contract_name c acc
      | _ -> acc) StringMap.empty acc

let filter_non_init acc =
  List.filter (function
      | Syn_init _ -> false
      | _ -> true
    ) acc

let inline_of_attributes = function
  | [ { txt = "inline"} , PStr [] ] -> InForced
  | [ { txt = "noinline"} , PStr [] ] -> InDont
  | _ -> InAuto

let rec translate_code contracts env exp =
  let loc = loc_of_loc exp.pexp_loc in
  let desc =
    match exp with
    | { pexp_desc = Pexp_extension ( { txt = "type" }, PTyp pty ) } ->
      let ty = translate_type env pty in
      Type ty

    | { pexp_desc = Pexp_ident ( { txt = var } ) } ->
      Var (str_of_id var)

    | { pexp_desc = Pexp_field (exp, { txt = label }) } ->
      let field = str_of_id label in
      let record = translate_code contracts env exp in
      Project { field; record }

    | { pexp_desc = Pexp_setfield (exp, { txt = label }, arg) } ->
      let field = str_of_id label in
      let record = translate_code contracts env exp in
      let set_val = translate_code contracts env arg in
      let rec set_field record loc field set_val =
        match record.desc with
        | Project p ->
          let set_val = mk ~loc (SetField { record; field; set_val }) in
          set_field p.record record.loc p.field set_val
        | _ -> SetField { record; field; set_val } in
      set_field record loc field set_val

    | { pexp_desc = Pexp_ifthenelse (e1, e2, None) } ->
      If { cond = translate_code contracts env e1;
           ifthen = translate_code contracts env e2;
           ifelse = mk ~loc (Const { ty = Tunit; const = CUnit }) }
    | { pexp_desc = Pexp_ifthenelse (e1, e2, Some e3) } ->
      If { cond = translate_code contracts env e1;
           ifthen = translate_code contracts env e2;
           ifelse = translate_code contracts env e3 }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Account", "transfer") } },
            ([_; _] as args));
        pexp_loc } ->
      let dest, amount =
        match order_labelled_args pexp_loc ["dest"; "amount"] args with
        | [d; a] -> d, a
        | _ -> assert false in
      Transfer { dest = translate_code contracts env dest;
                 amount = translate_code contracts env amount }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "call") } },
            ([_; _; _] as args));
        pexp_loc } ->
      let contract, amount, arg =
        match order_labelled_args pexp_loc
                ["dest"; "amount"; "parameter"] args
        with
        | [c; t; a] -> c, t, a
        | _ -> assert false in
      Call { contract = translate_code contracts env contract;
             amount = translate_code contracts env amount;
             entry = None;
             arg = translate_code contracts env arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "call") } },
            ([_; _; _; _] as args));
        pexp_loc } ->
      let contract, amount, entry, arg =
        match order_labelled_args pexp_loc
                ["dest"; "amount"; "entry"; "parameter"] args
        with
        | [c; t; { pexp_desc = Pexp_ident { txt = e }}; a] ->
          c, t, str_of_id e, a
        | _ -> error_loc pexp_loc "wrong arguments" in
      Call { contract = translate_code contracts env contract;
             amount = translate_code contracts env amount;
             entry = Some entry;
             arg = translate_code contracts env arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "create") } },
            ([_; _; _; _; _; _; _] as args));
        pexp_loc } ->
      let (manager_exp, delegate_exp, spendable_exp,
           delegatable_exp, amount_exp, storage_exp, contract) =
        match order_labelled_args pexp_loc
                ["manager"; "delegate"; "spendable"; "delegatable";
                 "amount"; "storage"; "code" ] args
        with
        | [d; m; de; s; a; st;
           { pexp_desc = Pexp_pack {
                 pmod_desc = Pmod_ident { txt = c };
                 pmod_loc } }] ->
          let c = str_of_id c in
          let c =
            try StringMap.find c contracts
            with Not_found -> unbound_contract pmod_loc c in
          (d, m, de, s, a, st, c)
        | [d; m; de; s; a; st;
           { pexp_desc = Pexp_pack {
                 pmod_desc = Pmod_structure structure;
                 pmod_loc } }] ->
          let inner_env = mk_inner_env env "_dummy_" in
          let inner_acc = StringMap.bindings contracts
                          |> List.map (fun (_, c) -> Syn_contract c) in
          let contract =
            translate_non_empty_contract inner_env inner_acc structure in
          (d, m, de, s, a, st, contract)
        | _ -> error_loc pexp_loc "wrong arguments for Contract.create" in
      CreateContract
        { args = [translate_code contracts env manager_exp;
                  translate_code contracts env delegate_exp;
                  translate_code contracts env spendable_exp;
                  translate_code contracts env delegatable_exp;
                  translate_code contracts env amount_exp;
                  translate_code contracts env storage_exp;
                 ];
          contract }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "create") } }, _);
        pexp_loc } ->
      error_loc pexp_loc
        "Wrong number or order of arguements for Contract.create.\n\
         Expected syntax is : Contract.create <manager> <delegate> \
         <delegatable> <spendable> <amount> <initial_storage> \
         (contract <ContractName>)"

    | { pexp_desc =
          Pexp_constraint (
            { pexp_desc =
                Pexp_apply (
                  { pexp_desc = Pexp_ident
                        { txt = Ldot(Lident "Contract", "at") } },
                  [
                    Nolabel, addr_exp;
                  ]) },
            pty) } ->

      let c_sig = match translate_type env pty with
        | Toption ((Tcontract csig)) -> csig
        | _ -> error_loc pty.ptyp_loc
                 "Contract.at type must be (contract C) option for some C"
      in
      ContractAt { arg =  translate_code contracts env addr_exp; c_sig }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(contract_id, "at") } },
            [
              Nolabel, addr_exp;
            ]) }
      when StringMap.mem (str_of_id contract_id) env.contract_types ->
      let c_sig = StringMap.find (str_of_id contract_id) env.contract_types in
      ContractAt { arg = translate_code contracts env addr_exp; c_sig }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident contract_name, "at") } },
            [
              Nolabel, addr_exp;
            ]) }
      when StringMap.mem contract_name contracts ->
      let c_sig = sig_of_contract (StringMap.find contract_name contracts) in
      ContractAt { arg =  translate_code contracts env addr_exp; c_sig }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "at") } },
            [
              Nolabel, addr_exp;
            ]);
        pexp_loc } ->
      (* let c_sig = Toption (Tvar (Ref.create
       *   { id = fresh_tv (); tyo = Some (Tpartial (Pcont [])) })) in
       * ContractAt { arg =  translate_code contracts env addr_exp; c_sig } *)
      error_loc pexp_loc
        "Contract.at must be annotated by the resulting contract type (option)"

    | { pexp_desc =
          Pexp_constraint (
            { pexp_desc =
                Pexp_apply (
                  { pexp_desc = Pexp_ident
                        { txt = Ldot(Lident "Bytes", "unpack") } },
                  [
                    Nolabel, exp;
                  ]) },
            pty) } ->

      let ty = match translate_type env pty with
        | Toption p -> p
        | _ -> error_loc pty.ptyp_loc
                 "Bytes.unpack type must be 'a option for some 'a"
      in
      Unpack { arg = translate_code contracts env exp; ty }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Bytes", "unpack") } },
            [
              Nolabel, exp;
            ]);
        pexp_loc } ->
      let ty = fresh_tvar () in
      Unpack { arg = translate_code contracts env exp; ty }

    | { pexp_desc = Pexp_let (Nonrecursive, [ {
        pvb_pat = pat;
        pvb_expr = var_exp;
        pvb_attributes = attrs;
      } ], body) } ->
      let bnd_val = translate_code contracts env var_exp in
      let body = translate_code contracts env body in
      let bnd_var, ty, body = deconstruct_pat env pat body in
      let inline = inline_of_attributes attrs in
      let bnd_val =
        match pat.ppat_desc with
        | Ppat_constraint _ -> mk ~loc (TypeAnnot { e = bnd_val; ty = ty })
        | _ -> bnd_val
      in
      Let { bnd_var; inline; bnd_val; body }

    (* Special (limited) form for recursive functions:
       let rec f = fun ... -> ... f ... *)
    | { pexp_desc = Pexp_let (Recursive, [ {
        pvb_pat = { ppat_desc = Ppat_var { txt = fun_name; loc = name_loc } };
        pvb_expr = { pexp_desc = Pexp_fun (Nolabel, None, pat, ({
            pexp_desc = fun_expr_desc } as fun_expr));
            pexp_loc = fun_loc;
          };
        pvb_attributes = attrs;
      } ], body) } ->
      let fun_body, ret_ty = match fun_expr_desc with
        | Pexp_constraint (fun_body, ret_ty) ->
          fun_body, translate_type env ret_ty
        | _ -> fun_expr, fresh_tvar ()
      in
      let fun_body = translate_code contracts env fun_body in
      let arg_name, arg_ty, fun_body = deconstruct_pat env pat fun_body in
      let is_rec =
        StringSet.mem fun_name
          (StringSet.remove arg_name.nname
             (LiquidBoundVariables.bv fun_body)) in
      if not is_rec then LiquidLoc.warn loc (NotRecursive fun_name);
      let recursive = if is_rec then Some fun_name else None in
      let lam = mk ~loc:(loc_of_loc fun_loc)
          (Lambda { arg_name; arg_ty; body = fun_body; ret_ty; recursive }) in
      let inline = inline_of_attributes attrs in
      let body = translate_code contracts env body in
      let bnd_var = { nname = fun_name; nloc = loc_of_loc name_loc } in
      Let { bnd_var; inline; bnd_val = lam; body }

    | { pexp_desc = Pexp_sequence (exp1, exp2) } ->
      Seq (translate_code contracts env exp1, translate_code contracts env exp2)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  ( { txt = (Lident "failwith"
                            | Ldot(Lident "Current", "failwith")) })},
            [
              Nolabel, err
            ]);
        pexp_loc
      } ->
      let arg = translate_code contracts env err in
      Failwith arg

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                    "loop");
                                         loc } ) },
            [
              Nolabel, { pexp_desc =
                           Pexp_fun (Nolabel,None,
                                     pat,
                                     body
                                    ) };
              Nolabel, arg
            ]) } ->
      let body = translate_code contracts env body in
      let arg = translate_code contracts env arg in
      let arg_name, _, body = deconstruct_pat env pat body in
      Loop { arg_name; body; arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                    "left");
                                         loc } ) },
            (Nolabel, { pexp_desc =
                          Pexp_fun (Nolabel,None,
                                    pat,
                                    body
                                   ) }) ::
            (Nolabel, arg) ::
            rest
          ) } ->
      let body = translate_code contracts env body in
      let arg = translate_code contracts env arg in
      let arg_name, _, body = deconstruct_pat env pat body in
      begin match rest with
        | [ Nolabel, acc ] ->
          let acc = translate_code contracts env acc in
          LoopLeft { arg_name; body; arg; acc = Some acc }
        | [] ->
          LoopLeft { arg_name; body; arg; acc = None }
        | _ -> error_loc loc "wrong number of arguments for Loop.left"
      end

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                    "left");
                                         loc } ) }, _) } ->
      error_loc loc "wrong of arguments for Loop.left"

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "iter") } ) },
            [
              Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".iter") ->
      let body = translate_code contracts env body in
      let arg = translate_code contracts env arg in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".iter") in
      let acc = mk ~loc (Const { ty = Tunit; const =  CUnit }) in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "fold") } ) },
            [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".fold") ->
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let body = translate_code contracts env body in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".fold") in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_coll,
                                                    "map") } ) },
            [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg;
            ]) }
      when LiquidTypes.is_map_primitive (map_coll^".map") ->
      let arg = translate_code contracts env arg in
      let body = translate_code contracts env body in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.map_primitive_of_string (map_coll^".map") in
      Map { prim; arg_name; body; arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_fold_coll,
                                                    "map_fold") } ) },
            [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_map_fold_primitive (map_fold_coll^".map_fold") ->
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let body = translate_code contracts env body in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.map_fold_primitive_of_string (map_fold_coll^".map_fold") in
      MapFold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "iter");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".iter") ->
      let f = translate_code contracts env f_exp in
      let vloc = loc_of_loc vloc in
      let arg_name = "_iter_arg" in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body = mk ~loc (Apply { prim = Prim_exec; args = [arg_var; f] }) in
      let arg = translate_code contracts env arg in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".iter") in
      let acc = mk ~loc (Const { ty = Tunit; const =  CUnit }) in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "fold");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".fold") ->
      let f = translate_code contracts env f_exp in
      let arg_name = "_fold_arg" in
      let vloc = loc_of_loc vloc in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body = mk ~loc (Apply { prim = Prim_exec; args = [arg_var; f] }) in
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".fold") in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_coll,
                                                    "map");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
            ]) }
      when LiquidTypes.is_map_primitive (map_coll^".map") ->
      let f = translate_code contracts env f_exp in
      let arg_name = "_map_arg" in
      let vloc = loc_of_loc vloc in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body = mk ~loc (Apply { prim = Prim_exec; args = [arg_var; f] }) in
      let arg = translate_code contracts env arg in
      let prim = LiquidTypes.map_primitive_of_string (map_coll^".map") in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      Map { prim; arg_name; body; arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_fold_coll,
                                                    "map_fold");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_map_fold_primitive (map_fold_coll^".map_fold") ->
      let f = translate_code contracts env f_exp in
      let arg_name = "_map_fold_arg" in
      let vloc = loc_of_loc vloc in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body = mk ~loc (Apply { prim = Prim_exec; args = [arg_var; f] }) in
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let prim =
        LiquidTypes.map_fold_primitive_of_string (map_fold_coll^".map_fold") in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      MapFold { prim; arg_name; body; arg; acc }

    | { pexp_desc = Pexp_apply (
        { pexp_desc = Pexp_ident { txt = Ldot(Lident "Account", "create") } },
        args);
        pexp_loc } ->
      let manager, delegate, delegatable, amount =
        match order_labelled_args pexp_loc
                ["manager"; "delegate"; "delegatable"; "amount"] args
        with
        | [m; d; de; a] -> (m, d, de, a)
        | _ -> error_loc pexp_loc "wrong arguments for Account.create" in
      Apply { prim = Prim_create_account;
              args = [
                translate_code contracts env manager;
                translate_code contracts env delegate;
                translate_code contracts env delegatable;
                translate_code contracts env amount;
              ] }

    (* special case for contract call with contract.entry param ~amount *)
    | { pexp_desc = Pexp_apply (
        { pexp_desc = Pexp_field (contract, { txt = Lident entry }) },
        ( [Nolabel, param; Labelled "amount", amount]
        | [Labelled "amount", amount; Nolabel, param] )
      ) } ->
      Call { contract = translate_code contracts env contract;
             amount = translate_code contracts env amount;
             entry = Some entry;
             arg = translate_code contracts env param }

    | { pexp_desc = Pexp_apply (exp, args) } ->
      let exp = translate_code contracts env exp in
      Apply { prim = Prim_unknown;
              args = exp :: List.map (function
                  | (Nolabel, exp) -> translate_code contracts env exp
                  | (_, { pexp_loc }) -> error_loc pexp_loc "in arg"
                ) args
            }

    | { pexp_desc = Pexp_extension (
        { txt = "nat" },
        PStr [{ pstr_desc = Pstr_eval (
            { pexp_desc = Pexp_match (e, cases); pexp_loc },
            [])
          }]
      )} ->
      let arg = translate_code contracts env e in
      let cases = List.map (translate_case contracts env) cases in
      let plus_name, ifplus, minus_name, ifminus = match cases with
        | [ CConstr ("Plus", [p]), ifplus, ploc;
            CConstr ("Minus", [m]), ifminus, mloc ]
        | [ CConstr ("Minus", [m]), ifminus, mloc;
            CConstr ("Plus", [p]), ifplus, ploc ] ->
          { nname = p; nloc = ploc}, ifplus, { nname = m; nloc = mloc }, ifminus
        | [ CConstr ("Plus", [p]), ifplus, ploc; CAny, ifminus, mloc ] ->
          { nname = p; nloc = ploc}, ifplus, { nname = "_"; nloc = mloc}, ifminus
        | [ CConstr ("Minus", [m]), ifminus, mloc; CAny, ifplus, ploc ] ->
          { nname = "_"; nloc = ploc}, ifplus, { nname = m; nloc = mloc }, ifminus
        | _ -> error_loc pexp_loc "match%%nat patterns are Plus _, Minus _"
      in
      MatchNat { arg; plus_name; ifplus; minus_name; ifminus }

    | { pexp_desc = Pexp_match (e, cases) } ->
      let arg = translate_code contracts env e in
      let cases = List.map (translate_case contracts env) cases in
      begin
        match cases with
        | [ CConstr ("None", []), ifnone, _; CConstr ("Some", [s]), ifsome, sloc ]
        | [ CConstr ("Some", [s]), ifsome, sloc; CConstr ("None", []), ifnone, _ ]
        | [ CConstr ("Some", [s]), ifsome, sloc; CAny, ifnone, _ ] ->
          MatchOption {arg; ifnone; some_name = { nname = s; nloc = sloc }; ifsome }
        | [ CConstr ("None", []), ifnone, _; CAny, ifsome, sloc ] ->
          MatchOption {arg; ifnone;
                       some_name = { nname = "_"; nloc = sloc }; ifsome }

        | [ CConstr ("[]", []), ifnil, _; CConstr ("::", [h; t]), ifcons, htloc ]
        | [ CConstr ("::", [h; t]), ifcons, htloc; CConstr ("[]", []), ifnil, _ ]
        | [ CConstr ("::", [h; t]), ifcons, htloc; CAny, ifnil, _ ] ->
          MatchList { arg;
                      head_name = { nname = h; nloc = htloc };
                      tail_name = { nname = t; nloc = htloc };
                      ifcons; ifnil }
        | [ CConstr ("[]", []), ifnil, _; CAny, ifcons, nloc ] ->
          MatchList { arg;
                      head_name = { nname = "_head"; nloc };
                      tail_name = { nname = "_tail"; nloc };
                      ifcons; ifnil }

        | _ ->
          let cases = List.map (fun (c, e, _) -> (c, e)) cases in
          MatchVariant { arg; cases }
      end

    | { pexp_desc = Pexp_fun (Nolabel, None, pat, body_exp) } ->
      let body_exp = translate_code contracts env body_exp in
      let arg_name, arg_ty, body = deconstruct_pat env pat body_exp in
      Lambda { arg_name; arg_ty; body;
               ret_ty = Tunit; (* not yet inferred *)
               recursive = None }

    | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
      let fields =
        List.map (fun ({ txt = label }, exp) ->
            str_of_id label, translate_code contracts env exp
          ) lab_x_exp_list in
      Record fields

    | exp ->
      match translate_const env exp with
      | _, None -> error_loc exp.pexp_loc "constant needs a type annotation"
      | const, Some ty -> Const { ty; const }
      | exception NotAConstant ->
        match exp with
        | { pexp_desc = Pexp_construct (
            { txt = Lident "Some" }, Some args) } ->
          Apply { prim = Prim_Some;
                  args = [translate_code contracts env args] }

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
          Apply { prim = Prim_Cons;
                  args = [translate_code contracts env a;
                          mk ~loc (Const { ty = fresh_tvar (); const =  CList [] }) (* XXX ? *)
                         ] }

        | { pexp_desc = Pexp_construct (
            { txt = Lident "::" },
            Some { pexp_desc = Pexp_tuple [a;b]}) } ->
          Apply { prim = Prim_Cons;
                  args = [translate_code contracts env a;
                          translate_code contracts env b] }


        | { pexp_desc = Pexp_construct ({ txt = Lident "Left" }, args) } ->
          Constructor { constr = Left (fresh_tvar ());
                        arg = match args with
                          | None -> mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }

        | { pexp_desc = Pexp_construct ({ txt = Lident "Right" }, args) } ->
          Constructor { constr = Right (fresh_tvar ());
                        arg = match args with
                          | None -> mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }


        | { pexp_desc = Pexp_construct ({ txt = lid }, args) } ->
          let lid = str_of_id lid in
          begin
            try
              let _ty_name = find_constr lid env in
              Constructor { constr = Constr lid;
                            arg = match args with
                              | None ->
                                mk ~loc (Const { ty = Tunit; const = CUnit })
                              | Some arg -> translate_code contracts env arg }
            with Not_found -> match lid with
              | "Map" | "Set" | "BigMap" ->
                error_loc exp.pexp_loc
                  "constructor %s can only be used with a constant argument"
                  lid
              | _ ->
                error_loc exp.pexp_loc "unknown constructor: %s" lid
          end

        | { pexp_desc =
              Pexp_constraint (
                { pexp_desc =
                    Pexp_construct (
                      { txt = Lident "Left" }, args) },
                pty
              ) } ->
          let right_ty = match pty with
            | { ptyp_desc = Ptyp_constr (
                { txt = Lident "variant" },
                [ left_ty; right_ty ])} ->
              translate_type env right_ty
            | _ -> match translate_type env pty with
              | Tor (_, right_ty) -> right_ty
              | _ -> error_loc pty.ptyp_loc
                       "Type of Left must be a variant"
          in
          Constructor { constr = Left right_ty;
                        arg = match args with
                          | None ->
                            mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }

        | { pexp_desc =
              Pexp_constraint (
                { pexp_desc =
                    Pexp_construct (
                      { txt = Lident "Right" }, args) },
                pty
              ) } ->
          let left_ty = match pty with
            | { ptyp_desc = Ptyp_constr (
                { txt = Lident "variant" },
                [ left_ty; right_ty ])} ->
              translate_type env left_ty
            | _ -> match translate_type env pty with
              | Tor (left_ty, _) -> left_ty
              | _ -> error_loc pty.ptyp_loc
                       "Type of Right must be a variant"
          in
          Constructor { constr = Right left_ty;
                        arg = match args with
                          | None ->
                            mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }

        (* TODO *)
        | { pexp_desc = Pexp_tuple [exp] } ->
          (translate_code contracts env exp).desc

        | { pexp_desc = Pexp_tuple exps } ->
          let exps = List.map (translate_code contracts env) exps in
          begin
            try
              let tys, csts = List.split (
                  List.map (
                    function
                    | { desc = Const { ty; const }} -> (ty, const)
                    | _ -> raise Exit
                  ) exps)
              in
              Const { ty = Ttuple tys; const = CTuple csts }
            with Exit ->
              Apply { prim = Prim_tuple; args = exps }
          end

        | { pexp_desc = Pexp_constraint (exp, ty); pexp_loc } ->
          TypeAnnot { e = translate_code contracts env exp;
                      ty = translate_type env ty }

        | { pexp_loc } ->
          error_loc pexp_loc
            "in expression %s"
            (LiquidPrinter.Syntax.string_of_expression exp)


  (*
    | { pexp_desc = Pexp_construct ({ txt = Lident id }, None) } ->
       todo_loc ("constructor " ^ id) exp.pexp_loc
   *)
  in
  mk ~loc desc

and translate_case contracts env case : (pattern * syntax_exp * location) =
  let var_of_pat = function
    | { ppat_desc = Ppat_var { txt = var } } -> var
    | { ppat_desc = Ppat_any } -> "_"
    | { ppat_desc = Ppat_construct _; ppat_loc } ->
      error_loc ppat_loc "cannot match deep"
    | { ppat_loc } ->
      error_loc ppat_loc "bad pattern"
  in
  match case.pc_guard with
  | Some e  ->
    error_loc e.pexp_loc "when forbidden"
  | None ->
    let e = case.pc_rhs in
    let e = translate_code contracts env e in
    match case.pc_lhs with
    | { ppat_desc = Ppat_any; ppat_loc} ->
      (CAny, e, loc_of_loc ppat_loc)
    | { ppat_desc =
          Ppat_construct (
            { txt = Lident ("Some" | "::" | "Plus" | "Minus" as c) },
            None);
        ppat_loc }  ->
      error_loc ppat_loc "Constructor %S takes arguments, was given 0" c
    | { ppat_desc =
          Ppat_construct ( { txt = Lident ("None" | "[]" as c) }, Some _);
        ppat_loc }  ->
      error_loc ppat_loc "Constructor %S takes no arguments" c
    | { ppat_desc = Ppat_construct ( { txt = name } , None); ppat_loc }  ->
      (CConstr (str_of_id name, []), e, loc_of_loc ppat_loc)
    | { ppat_desc =
          Ppat_construct (
            { txt = Lident "::" } ,
            Some { ppat_desc = Ppat_tuple [ p1; p2 ]}
          );
        ppat_loc }  ->
      (CConstr ("::", [var_of_pat p1; var_of_pat p2]), e, loc_of_loc ppat_loc)

    | { ppat_desc = Ppat_construct ({ txt = name } , Some pat); ppat_loc }  ->
      let var_name, _, e = deconstruct_pat env pat e in
      (CConstr (str_of_id name, [var_name.nname]), e, loc_of_loc ppat_loc)

    | { ppat_loc } ->
      error_loc ppat_loc "bad pattern"

and translate_entry name env contracts head_exp mk_parameter mk_storage =
  match head_exp with
  | { pexp_desc = Pexp_fun (Nolabel, None, param_pat, head_exp) }
    when mk_parameter = None ->
    let mk_param = deconstruct_pat env param_pat in
    translate_entry name env contracts head_exp (Some mk_param) mk_storage

  | { pexp_desc = Pexp_fun (Nolabel, None, storage_pat, head_exp);
      pexp_loc }
    when mk_storage = None && mk_parameter <> None ->
    let mk_storage code =
      let storage_name, storage_ty, code =
        deconstruct_pat env storage_pat code in
      match storage_pat.ppat_desc with
      | Ppat_constraint _ | Ppat_construct _ ->
        begin try
            let s = find_type "storage" env [] in
            if not @@ eq_types s storage_ty then
              LiquidLoc.raise_error ~loc:storage_name.nloc
                "storage argument %s for entry point %s must be the same type \
                 as contract storage" storage_name.nname name;
            (storage_name, code)
          with Not_found ->
            error_loc pexp_loc "type storage is required but not provided"
        end
      | _ -> (storage_name, code)
    in
    translate_entry name env contracts head_exp mk_parameter (Some mk_storage)

  | { pexp_desc = Pexp_fun (Nolabel, None, _, _);
      pexp_loc }  ->
    LiquidLoc.raise_error ~loc:(loc_of_loc pexp_loc)
      "entry point %s accepts only two arguments, \
       i.e. one parameter, one storage" name

  | { pexp_loc } when mk_parameter = None || mk_storage = None ->
    LiquidLoc.raise_error ~loc:(loc_of_loc pexp_loc)
      "entry point %s needs two arguments" name

  | { pexp_desc = Pexp_constraint (head_exp, return_type); pexp_loc } ->
    begin match translate_type env return_type with
      | Ttuple [ ret_ty; sto_ty ] ->
        let storage = find_type "storage" env [] in
        if not @@ eq_types sto_ty storage then
          error_loc pexp_loc
            "Second component of return type must be identical to storage type";
        if ret_ty <> Tlist Toperation then
          error_loc pexp_loc
            "First component of return type must be an operation list"
      | _ ->
        error_loc pexp_loc
          "return type must be a product of \"operation list\" \
           and the storage type"
    end;
    translate_entry name env contracts head_exp mk_parameter mk_storage

  | exp ->
    let code = translate_code contracts env exp in
    let parameter_name, parameter, code = match mk_parameter with
      | Some mk -> mk code
      | None -> assert false in
    let storage_name, code = match mk_storage with
      | Some mk -> mk code
      | None -> assert false in
    {
      entry_sig = {
        entry_name = name;
        parameter;
        parameter_name = parameter_name.nname;
        storage_name = storage_name.nname;
      };
      code;
    }

and translate_initial_storage env init_name contracts exp args =
  match exp with
  | { pexp_desc =
        Pexp_fun (
          Nolabel, None,
          { ppat_desc =
              Ppat_constraint(
                { ppat_desc = Ppat_var { txt = arg; loc } },
                arg_type)
          },
          exp) } ->
    translate_initial_storage env init_name contracts exp
      ((arg, loc_of_loc loc, translate_type env arg_type) :: args)

  | { pexp_desc =
        Pexp_fun (
          Nolabel, None,
          { ppat_desc = Ppat_var { txt = arg; loc } }, exp) } ->
    translate_initial_storage env init_name contracts exp
      ((arg, loc_of_loc loc, (fresh_tvar ())) :: args)

  | _ ->
    let init_body = translate_code contracts env exp in
    { init_name;
      init_args = List.rev args;
      init_body }

and renamespace env old_name new_name =
  let prefix = old_name ^ "." in
  let name_in_old s = LiquidMisc.has_prefix ~prefix s in
  let fold_add env proj =
    fold_env_proj (fun s t acc ->
        if name_in_old s then StringMap.add s t acc else acc
      ) (proj env) env proj in
  (* match name_in_contract s with
       *   | None -> acc
       *   | Some s ->
       *     (\* StringMap.add s t acc *\)
       *     StringMap.add (String.concat "." [new_contract_name; s]) t acc
         * ) *)
  (* Make a new empty environment *)
  let env = mk_inner_env env new_name in
  (* copy old namespace to new env *)
  env.types <- fold_add env (fun env -> env.types);
  env.contract_types <- fold_add env (fun env -> env.contract_types);
  env.labels <- fold_add env (fun env -> env.labels);
  env.constrs <- fold_add env (fun env -> env.constrs);
  let rename new_name s =
    match LiquidMisc.remove_prefix ~prefix s with
    | None -> s
    | Some s -> String.concat "." [new_name; s] in
  (* Lift to upper level and rename *)
  lift_env rename env |> ignore;
  ()


and translate_signature contract_type_name env acc ast =
  match ast with
  | [] ->
    { sig_name = Some contract_type_name;
      entries_sig = List.rev acc }

  | { psig_desc = Psig_type (
      Recursive, [
        { ptype_name = { txt = ty_name; loc=name_loc };
          ptype_params = params;
          ptype_cstrs = [];
          ptype_private = Public;
          ptype_manifest = None;
          ptype_attributes = [];
          ptype_loc;
          ptype_kind; (* Ptype_record labels;*)
        }
      ]) } :: ast ->
    if StringMap.mem ty_name env.types then
      error_loc name_loc "type %s already defined" ty_name;
    begin match ptype_kind with
      | Ptype_record labels ->
        if List.length labels < 2 then begin
          error_loc ptype_loc "record must have at least two fields"
        end;
        translate_record ~loc:ptype_loc ty_name params labels env;
      | Ptype_abstract -> ()
      | Ptype_open -> error_loc ptype_loc "bad type definition"
      | Ptype_variant constrs ->
        if List.length constrs < 2 then begin
          error_loc ptype_loc "variant type must have at least two constructors"
        end;
        translate_variant ~loc:ptype_loc ty_name params constrs env;
    end;
    translate_signature contract_type_name env acc ast

  (* type alias *)
  | { psig_desc = Psig_type (
      Recursive,
      [
        { ptype_name = { txt = ty_name; loc=name_loc };
          ptype_params = params;
          ptype_cstrs = [];
          ptype_private = Public;
          ptype_manifest = Some ct;
          ptype_attributes = [];
          ptype_loc;
          ptype_kind;
        }
      ]) } :: ast ->
    let ty = translate_type env ct in
    add_type_alias ~loc:ptype_loc ty_name params ty env;
    translate_signature contract_type_name env acc ast

  | { psig_desc = Psig_extension (
      ({ txt = "entry" }, PSig [{
           psig_desc = Psig_value {
               pval_name = { txt = entry_name; loc = name_loc };
               pval_type = {
                 ptyp_desc = Ptyp_arrow (param_label, param_ty, ret_ty)
               };
               pval_prim = [];
               pval_attributes = [];
               pval_loc;
             }}
         ]), [])} :: ast ->
    if List.mem entry_name reserved_keywords then
      error_loc name_loc "entry point %S forbidden" entry_name;
    if List.exists (fun e -> e.entry_name = entry_name) acc then
      error_loc name_loc "entry point %S is already declared" entry_name;
    let parameter_name = match param_label with
      | Nolabel -> "parameter"
      | Optional _ -> error_loc pval_loc "cannot have optional parameter"
      | Labelled p -> p in
    let parameter = translate_type env param_ty in
    let storage_name, ret_ty = match ret_ty.ptyp_desc with
      | Ptyp_arrow (Nolabel, stora_ty, ret_ty) ->
        "storage", ret_ty
      | Ptyp_arrow (Optional _, stora_ty, _) ->
        error_loc ret_ty.ptyp_loc "cannot have optional storage"
      | Ptyp_arrow (Labelled s, stora_ty, ret_ty) ->
        s, ret_ty
      | Ptyp_any ->
        "storage", ret_ty
      | _ ->
        error_loc ret_ty.ptyp_loc
          "must be an arrow type storage -> (operation list * storage)"
    in
    begin match ret_ty.ptyp_desc with
      | Ptyp_any -> ()
      | Ptyp_tuple [ ret_op ;
                     { ptyp_desc =
                         Ptyp_constr ({ txt = Lident "storage" }, []) }] ->
        begin match translate_type env ret_op with
          | Tlist Toperation -> ()
          | _ -> error_loc ret_op.ptyp_loc
                   "entry must return operation list as first component"
        end
      | _ -> error_loc ret_ty.ptyp_loc
               "entry must return (operation list * storage)"
    end;
    let entry = { entry_name; parameter; parameter_name; storage_name } in
    translate_signature contract_type_name env (entry :: acc) ast

  | { psig_desc = Psig_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_signature signature
              }
          };
      psig_loc;
    } :: ast ->
    let inner_env = mk_inner_env env contract_type_name in
    let contract_sig =
      translate_signature contract_type_name inner_env [] signature in
    if contract_sig.entries_sig = [] then
      error_loc psig_loc
        "Contract type %s has no entry points (use struct to define namespace)"
        contract_type_name;
    let lift_type = lift_inner_env inner_env in
    let contract_sig =
      { contract_sig with
        entries_sig = List.map (fun es ->
            { es with parameter = lift_type es.parameter }
          ) contract_sig.entries_sig
      } in
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_signature contract_type_name env acc ast

  | { psig_loc } as ast :: _ ->
    if !LiquidOptions.verbosity > 0 then
      error_loc psig_loc "in signature:\n%a" Printast.interface [ast]
    else
      error_loc psig_loc "in signature"



and translate_structure env acc ast : syntax_contract option =
  match ast with
  | { pstr_desc = Pstr_primitive {
      pval_name = { txt = prim_name; loc = prim_loc };
      pval_type = prim_type; pval_prim = [minst];
      pval_attributes = prim_attr } } :: ast ->
    if List.mem prim_name reserved_keywords then
      error_loc prim_loc "Primitive name %S forbidden" prim_name;
    if StringMap.mem prim_name env.ext_prims then
      error_loc prim_loc "Primitive %S already defined" prim_name;
    if List.exists (function
        | Syn_value (n, _, _) -> n = prim_name
        | _ -> false) acc
    then
      error_loc prim_loc "Top-level identifier %S already defined" prim_name;
    let tvs, atys, rtys = translate_ext_type env prim_type in
    let valid_in_external = function
      | Trecord _ | Tsum _ | Tclosure _ | Tfail | Ttuple (_ :: _ :: _ :: _) ->
        error_loc prim_loc
          "Primitive %S can only use standard Michelson types" prim_name
      | _ -> ()
    in
    List.iter valid_in_external atys;
    List.iter valid_in_external rtys;
    let effect =  List.exists (fun (a, _) -> a.txt = "effect") prim_attr in
    let nb_arg = List.length atys in
    let nb_ret = List.length rtys in
    let atys = if atys = [] then [Tunit] else atys in
    let rty = match rtys with
      | [] -> Tunit
      | [ty] -> ty
      | _ -> Ttuple rtys
    in
    env.ext_prims <- StringMap.add prim_name
        { tvs; atys; rty; effect; nb_arg; nb_ret; minst } env.ext_prims;
    translate_structure env acc ast

  | { pstr_desc =
        Pstr_extension
          (({ Asttypes.txt = "version" },
            PStr [{ pstr_desc = Pstr_eval (exp,[])}]),[])
    } :: ast ->
    check_version exp;
    translate_structure env acc ast

  | { pstr_desc =
        Pstr_extension
          (({ txt = "init" },
            PStr
              [{ pstr_desc =
                   Pstr_value (
                     Nonrecursive,
                     [ {
                       pvb_pat = { ppat_desc = Ppat_var { txt = init_name } };
                       pvb_expr = sto_exp;
                     }
                     ]) } ]
           ), []);
      pstr_loc } :: ast
    ->
    if List.exists (function Syn_init _ -> true | _ -> false) acc then
      error_loc pstr_loc "Initial storage already defined";
    let init =
      Syn_init (translate_initial_storage env init_name
                  (filter_contracts acc) sto_exp [])
    in
    translate_structure env (init :: acc) ast

  | { pstr_desc =
        Pstr_extension
          (({ txt = "entry" },
            PStr
              [{ pstr_desc =
                   Pstr_value (
                     Nonrecursive,
                     [ {
                       pvb_pat = { ppat_desc =
                                     Ppat_var { txt = name; loc = name_loc } };
                       pvb_expr = head_exp;
                     }
                     ]) } ]
           ), []) } :: ast
    ->
    if List.mem name reserved_keywords then
      error_loc name_loc "entry point %S forbidden" name;
    if List.exists (function
        | Syn_entry e -> e.entry_sig.entry_name = name
        | _ -> false) acc then
      error_loc name_loc "entry point %S is already defined" name;
    let entry =
      Syn_entry (translate_entry name env
                   (filter_contracts acc) head_exp None None) in
    translate_structure env (entry :: acc) ast

  | { pstr_desc = (
      Pstr_value (
        Nonrecursive,
        [ {
          pvb_pat = { ppat_desc = Ppat_var { txt = var_name; loc = name_loc } };
          pvb_expr = var_exp;
          pvb_attributes = attrs;
        }
        ])); pstr_loc = f_loc } :: ast ->
    let exp = translate_code (filter_contracts acc) env var_exp in
    let inline = inline_of_attributes attrs in
    if List.mem var_name reserved_keywords then
      error_loc name_loc "top-level value %S forbidden" var_name;
    if StringMap.mem var_name env.ext_prims then
      error_loc name_loc "Top-level identifier %S already defined" var_name;
    if List.exists (function
        | Syn_value (n, _, _) -> n = var_name
        | _ -> false) acc
    then
      error_loc name_loc "Top-level value %S already defined" var_name;
    let value = Syn_value (var_name, inline, exp) in
    translate_structure env (value :: acc) ast


  | { pstr_desc = (
      Pstr_value (
        Recursive,
        [ {
          pvb_pat = { ppat_desc = Ppat_var { txt = fun_name; loc = name_loc } };
          pvb_expr = { pexp_desc = Pexp_fun (Nolabel, None, pat, ({
              pexp_desc = fun_expr_desc } as fun_expr));
              pexp_loc = fun_loc;
            };
          pvb_attributes = attrs;
        }
        ])); pstr_loc = f_loc } :: ast ->
    let contracts = filter_contracts acc in
    let fun_body, ret_ty = match fun_expr_desc with
      | Pexp_constraint (fun_body, ret_ty) ->
        fun_body, translate_type env ret_ty
      | _ -> fun_expr, fresh_tvar ()
    in
    (* let ret_ty = translate_type env ret_ty in *)
    let fun_body = translate_code contracts env fun_body in
    let arg_name, arg_ty, fun_body = deconstruct_pat env pat fun_body in
    let is_rec =
      StringSet.mem fun_name
        (StringSet.remove arg_name.nname
           (LiquidBoundVariables.bv fun_body)) in
    let loc = loc_of_loc fun_loc in
    if not is_rec then LiquidLoc.warn loc (NotRecursive fun_name);
    let recursive = if is_rec then Some fun_name else None in
    let lam = mk ~loc
        (Lambda { arg_name; arg_ty; body = fun_body; ret_ty; recursive }) in
    let inline = inline_of_attributes attrs in
    if List.mem fun_name reserved_keywords then
      error_loc name_loc "top-level value %S forbidden" fun_name;
    if StringMap.mem fun_name env.ext_prims then
      error_loc name_loc "Top-level identifier %S already defined" fun_name;
    if List.exists (function
        | Syn_value (n, _, _) -> n = fun_name
        | _ -> false) acc
    then
      error_loc name_loc "Top-level value %S already defined" fun_name;
    let value = Syn_value (fun_name, inline, lam) in
    translate_structure env (value :: acc) ast


  | { pstr_desc = Pstr_type (Recursive,
                             [
                               { ptype_name = { txt = ty_name; loc=name_loc };
                                 ptype_params = params;
                                 ptype_cstrs = [];
                                 ptype_private = Public;
                                 ptype_manifest = None;
                                 ptype_attributes = [];
                                 ptype_loc;
                                 ptype_kind; (* Ptype_record labels;*)
                               }
                             ]) } :: ast ->
    if StringMap.mem ty_name env.types then
      error_loc name_loc "type %s already defined" ty_name;
    begin match ptype_kind with
      | Ptype_record labels ->
        if List.length labels < 2 then begin
          error_loc ptype_loc "record must have at least two fields"
        end;
        translate_record ~loc:ptype_loc ty_name params labels env;
      | Ptype_abstract
      | Ptype_open -> error_loc ptype_loc "bad type definition"
      | Ptype_variant constrs ->
        if List.length constrs < 2 then begin
          error_loc ptype_loc "variant type must have at least two constructors"
        end;
        translate_variant ~loc:ptype_loc ty_name params constrs env;
    end;
    translate_structure env acc ast

  (* type alias *)
  | { pstr_desc = Pstr_type (Recursive,
                             [
                               { ptype_name = { txt = ty_name; loc=name_loc };
                                 ptype_params = params;
                                 ptype_cstrs = [];
                                 ptype_private = Public;
                                 ptype_manifest = Some ct;
                                 ptype_attributes = [];
                                 ptype_loc;
                                 ptype_kind;
                               }
                             ]) } :: ast ->
    let ty = translate_type env ct in
    add_type_alias ~loc:ptype_loc ty_name params ty env;
    translate_structure env acc ast

  (* contract types *)

  | { pstr_desc = Pstr_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_signature signature
              }
          };
      pstr_loc;
    } :: ast ->
    let inner_env = mk_inner_env env contract_type_name in
    let contract_sig =
      translate_signature contract_type_name inner_env [] signature in
    if contract_sig.entries_sig = [] then
      error_loc pstr_loc
        "Contract type %s has no entry points (use struct to define namespace)"
        contract_type_name;
    let lift_type = lift_inner_env inner_env in
    let contract_sig =
      { contract_sig with
        entries_sig = List.map (fun es ->
            { es with parameter = lift_type es.parameter }
          ) contract_sig.entries_sig
      } in
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_structure env acc ast

  (* Deactivate aliases for signatures *)
  (*
  | { pstr_desc = Pstr_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_ident { txt = id };
                pmty_loc;
              }
          }
    } :: ast ->
    let contract_sig =
      try find_contract_type (str_of_id id) env
      with Not_found ->
        error_loc pmty_loc "No contract type named %s knwon" (str_of_id id)
    in
    renamespace env (str_of_id id) contract_type_name;
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_structure env acc ast

  | { pstr_desc = Pstr_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_typeof {
                    pmod_desc = Pmod_ident { txt = c_name };
                    pmod_loc;
                  }
              }
          }
    } :: ast ->
    let contract =
      try StringMap.find (str_of_id c_name) (filter_contracts acc)
      with Not_found ->
        error_loc pmod_loc "No contract named %s knwon" (str_of_id c_name)
    in
    let contract_sig = sig_of_contract contract in
    renamespace env (str_of_id c_name) contract_type_name;
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_structure env acc ast
  *)

  (* contracts *)

  (* Deactivate aliases for contracts *)
  (*
  | { pstr_desc = Pstr_module {
      pmb_name = { txt = contract_name };
      pmb_expr = {
        pmod_desc = Pmod_ident { txt = Lident c_name; loc };
      }
    }
    } :: ast ->
    begin try
        let contract = StringMap.find c_name (filter_contracts acc) in
        let contract = Syn_contract { contract with contract_name } in
        renamespace env c_name contract_name;
        translate_structure env (contract :: acc) ast
      with Not_found ->
        unbound_contract loc c_name
    end
  *)

  | { pstr_desc = Pstr_module {
      pmb_name = { txt = contract_name };
      pmb_expr = {
        pmod_desc = Pmod_structure structure
      };
    }} :: ast ->
    let inner_env = mk_inner_env env contract_name in
    begin
      match translate_structure inner_env (filter_non_init acc) structure with
      | None ->
        lift_inner_env inner_env |> ignore;
        translate_structure env acc ast
      | Some contract ->
        match !LiquidOptions.main with
        | Some main when main = contract_name ->
          Some contract
        | _ ->
          let lift_type = lift_inner_env contract.ty_env in
          let contract_sig = sig_of_contract contract in
          let contract_sig =
            { sig_name = Some contract_name;
              entries_sig = List.map (fun es ->
                  { es with parameter = lift_type es.parameter }
                ) contract_sig.entries_sig
            } in
          (* let contract = { contract with storage = lift_type contract.storage } in *)
          (* Register contract type (with same name as contract) in environment *)
          env.contract_types <-
            StringMap.add contract_name contract_sig env.contract_types;
          translate_structure env (Syn_contract contract :: acc) ast
    end

  | [] -> pack_contract env (List.rev acc)

  | { pstr_loc = loc } as ast :: _ ->
    if !LiquidOptions.verbosity > 0 then
      error_loc loc "at toplevel:\n%a" Printast.implementation [ast]
    else
      error_loc loc "at toplevel"

and translate_non_empty_contract env acc ast =
  match translate_structure env acc ast with
  | None ->
    Location.raise_errorf
      "No entry point found for contract %s in file %S%!"
      env.contractname
      env.filename
  | Some contract -> contract

and pack_contract env toplevels =
  if not (List.exists (function Syn_entry _ -> true | _ -> false) toplevels)
  then None
  else
    let storage =
      try StringMap.find "storage" env.types []
      with Not_found ->
        LiquidLoc.raise_error
          ~loc:(LiquidLoc.loc_in_file env.filename)
          "type storage is required but not provided"
    in
    (* if not (List.exists (function Syn_entry _ -> true | _ -> false) toplevels)
     * then Location.raise_errorf "No entry point found for contract %s in file %S%!"
     *     env.contractname
     *     env.filename; *)
    let rec partition (contracts, values, entries, init) = function
      | Syn_value (v,i,e) :: r ->
        partition (contracts, (v,i,e) :: values, entries, init) r
      | Syn_contract c :: r -> partition (c :: contracts, values, entries, init) r
      | Syn_entry e :: r -> partition (contracts, values, e :: entries, init) r
      | Syn_init i :: r -> partition (contracts, values, entries, Some i) r
      | [] -> (List.rev contracts, List.rev values, List.rev entries, init) in
    let _contracts, values, entries, init =
      partition ([], [], [], None) toplevels in
    Some { contract_name = env.contractname;
           storage;
           values;
           entries;
           c_init = init;
           ty_env = env }


let predefined_constructors =
  List.fold_left (fun acc (constr, info) ->
      StringMap.add constr info acc) StringMap.empty
    (* These constructors are never looked-up in this
       map, hence we can provide wrong info. However, they
       need to be there to prevent the user from overriding
       them. *)
    [
      "Some", "'a option";
      "None", "'a option";
      "::", "'a list";
      "[]", "'a list";
      "Left", "('a, 'b) variant";
      "Right", "('a, 'b) variant";
      "Map", "('a, 'b) map";
      "Set", "'a set";
    ]


let filename_to_contract filename =
  String.capitalize_ascii
    (LiquidMisc.string_replace
       Filename.(basename filename |> remove_extension)
       '.' '_')

let initial_env filename =
  {
    types = StringMap.map (fun ty -> fun _ -> ty) predefined_types;
    contract_types = predefined_contract_types;
    labels = StringMap.empty;
    constrs = predefined_constructors;
    ext_prims = StringMap.empty;
    filename;
    top_env = None;
    contractname = filename_to_contract filename;
  }

let translate_exn exn =
  match exn with
  | Location.Error err ->
    let loc = loc_of_loc Location.(err.loc) in
    LiquidLoc.raise_error ~loc "%s"  Location.(err.msg)

  (* Syntax errors *)
  | Syntaxerr.Error err  ->
    let (loc, msg) =
      match err with
      | Syntaxerr.Unclosed(opening_loc, opening, closing_loc, closing) ->
        closing_loc,
        Printf.sprintf "Syntax error: '%s' expected" closing
      | Syntaxerr.Expecting (loc, nonterm) ->
        loc,
        Printf.sprintf "Syntax error: %s expected." nonterm
      | Syntaxerr.Not_expecting (loc, nonterm) ->
        loc,
        Printf.sprintf "Syntax error: %s not expected." nonterm
      | Syntaxerr.Applicative_path loc ->
        loc,
        "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
      | Syntaxerr.Variable_in_scope (loc, var) ->
        loc,
        Printf.sprintf "In this scoped type, variable '%s \
                        is reserved for the local type %s."
          var var
      | Syntaxerr.Other loc ->
        loc, "Syntax error"
      | Syntaxerr.Ill_formed_ast (loc, s) ->
        loc,
        Printf.sprintf "broken invariant in parsetree: %s" s
      | Syntaxerr.Invalid_package_type (loc, s) ->
        loc,
        Printf.sprintf "invalid package type: %s" s
    in
    let loc = loc_of_loc loc in
    LiquidLoc.raise_error ~loc "%s" msg

  | LiquidOCamlLexer.Error (error, oloc) ->
    let loc = loc_of_loc oloc in
    LiquidLoc.raise_error ~loc "%s"
      (Location.error_of_printer
         oloc
         LiquidOCamlLexer.report_error error).Location.msg
      LiquidOCamlLexer.report_error ppf error
  | _ -> raise exn


let translate ~filename ast =
  let env = initial_env filename in
  try
    let contract = translate_non_empty_contract env [] ast in
    begin match !LiquidOptions.main with
      | Some main when main <> contract.contract_name ->
        Format.eprintf "No contract named %s.@." main;
        exit 2;
      | _ -> ()
    end;
    contract
  with exn -> translate_exn exn

let mk_toplevel_env filename top_env =
  { types = StringMap.empty;
    contract_types = StringMap.empty;
    labels = StringMap.empty;
    constrs = StringMap.empty;
    ext_prims = StringMap.empty;
    filename;
    top_env = Some top_env;
    contractname = filename_to_contract filename;
  }

let translate_multi l =
  match List.rev l with
  | [] ->
    Format.eprintf "No contracts@.";
    exit 2
  | (filename, ast) :: r_others ->
    let top_env = initial_env filename in
    let exception Stop of syntax_contract in
    try
      let acc =
        List.fold_left (fun acc (filename, ast) ->
            let env = mk_toplevel_env filename top_env in
            match translate_structure env acc ast with
            | None ->
              lift_inner_env env |> ignore;
              acc
            | Some contract ->
              begin match !LiquidOptions.main with
                | Some main when main = contract.contract_name ->
                  Format.eprintf "Main contract %s@." contract.contract_name;
                  raise (Stop contract)
                | _ ->
                  Format.eprintf "Contract %s@." contract.contract_name;
              end;
              let lift_type = lift_inner_env env in
              let contract_sig = sig_of_contract contract in
              let contract_sig =
                { sig_name = Some contract.contract_name;
                  entries_sig = List.map (fun es ->
                      { es with parameter = lift_type es.parameter }
                    ) contract_sig.entries_sig
                } in
              (* Register contract type (with same name as contract) in environment *)
              env.contract_types <-
                StringMap.add contract.contract_name contract_sig
                  top_env.contract_types;
              Syn_contract contract :: acc
          ) [] (List.rev r_others)
      in
      let contract = translate_non_empty_contract top_env acc ast in
      Format.eprintf "Main contract %s@." contract.contract_name;
      begin match !LiquidOptions.main with
        | Some main when main <> contract.contract_name ->
          Format.eprintf "No contract named %s.@." main;
          exit 2;
        | _ -> ()
      end;
      contract
    with
    | Stop contract -> contract
    | exn -> translate_exn exn

let ocaml_of_file parser file =
  let ic = open_in file in
  try
    Location.input_name := file;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf file;
    let ast = parser lexbuf in
    close_in ic;
    ast
  with exn ->
    close_in ic;
    translate_exn exn

let read_file filename =
  let parse =
    if Filename.check_suffix filename ".reliq" then
      LiquidReasonParse.implementation
    else
      LiquidOCamlParse.implementation in
  try
    let ast, _comments =
      ocaml_of_file parse filename in
    ast
  with exn -> translate_exn exn

let translate_expression env expression =
  try
    translate_code StringMap.empty env expression
  with exn -> translate_exn exn

let ocaml_of_string ?(filename = "buffer") parser content =
  try
    Location.input_name := filename;
    let lexbuf = Lexing.from_string content in
    Location.init lexbuf filename;
    parser lexbuf
  with exn ->
    translate_exn exn

let structure_of_string ?filename impl =
  let str, _comments =
    ocaml_of_string ?filename LiquidParse.implementation impl in
  str

let expression_of_string ?filename s =
  ocaml_of_string ?filename LiquidParse.expression s

let translate_type env ty = translate_type env ty

let type_of_string ?filename s =
  ocaml_of_string ?filename LiquidParse.core_type s
