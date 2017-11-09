(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

exception InvalidFormat of string * string

type tez = { tezzies : string; centiles : string option }
type integer = { integer : string }

type encoded

type const =
  | CUnit
  | CBool of bool
  | CInt of integer
  | CNat of integer
  | CTez of tez
  | CTimestamp of string
  | CString of string
  | CKey of string
  | CSignature of string
  | CTuple of const list
  | CNone
  | CSome of const

  (* Map [ key_x_value_list ] or (Map [] : ('key,'value) map) *)
  | CMap of (const * const) list
  | CList of const list
  | CSet of const list

  | CLeft of const
  | CRight of const

  | CKey_hash of string

  | CRecord of (string * const) list
  | CConstr of string * const

 and datatype =
   (* michelson *)
  | Tunit
  | Tbool
  | Tint
  | Tnat
  | Ttez
  | Tstring
  | Ttimestamp
  | Tkey
  | Tkey_hash
  | Tsignature

  | Ttuple of datatype list

  | Trecord of (string * datatype) list
  | Tsum of (string * datatype) list

  | Toption of datatype
  | Tlist of datatype
  | Tset of datatype

  | Tmap of datatype * datatype
  | Tcontract of datatype * datatype
  | Tor of datatype * datatype
  | Tlambda of datatype * datatype

  (* liquidity extensions *)
  | Tclosure of (datatype * datatype) * datatype
  | Tfail
  | Ttype of string * datatype

let rec get_type ty = match ty with
  | Ttez | Tunit | Ttimestamp | Tint | Tnat | Tbool | Tkey | Tkey_hash
  | Tsignature | Tstring | Tfail -> ty
  | Ttuple tys ->
    let tys' = List.map get_type tys in
    if List.for_all2 (==) tys tys' then ty
    else Ttuple tys'
  | Tset t | Tlist t | Toption t ->
    let t' = get_type t in
    if t' == t then ty
    else begin match ty with
      | Tset t -> Tset t'
      | Tlist t -> Tlist t'
      | Toption t -> Toption t'
      | _ -> assert false
    end
  | Tor (t1, t2) | Tcontract (t1, t2) | Tlambda (t1, t2) | Tmap (t1, t2) ->
    let t1', t2' = get_type t1, get_type t2 in
    if t1 == t1' && t2 == t2' then ty
    else begin match ty with
      | Tor (t1, t2) -> Tor (t1', t2')
      | Tcontract (t1, t2) -> Tcontract (t1', t2')
      | Tlambda (t1, t2) -> Tlambda (t1', t2')
      | Tmap (t1, t2) -> Tmap (t1', t2')
      | _ -> assert false
    end
  | Tclosure  ((t1, t2), t3) ->
    let t1', t2', t3' = get_type t1, get_type t2, get_type t3 in
    if t1 == t1' && t2 == t2' && t3 == t3' then ty
    else Tclosure ((t1', t2'), t3')
  | Trecord ntys | Tsum ntys ->
    let change = ref false in
      let ntys' = List.map (fun (l, ty) ->
        let ty' = get_type ty in
        if ty' != ty then change := true;
        (l, ty')
      ) ntys in
    if !change then match ty with
      | Trecord _ -> Trecord ntys'
      | Tsum _ -> Tsum ntys'
      | _ -> assert false
    else ty

  | Ttype (name, t) -> get_type t

let first_alias ty =
  let rec aux acc ty = match acc, ty with
    | _, Ttype (name, ty) -> aux (Some name) ty
    | Some name, ty -> Some (name, ty)
    | None, _ -> None
  in
  aux None ty

let eq_types t1 t2 =
  get_type t1 = get_type t2


type 'exp contract = {
    parameter : datatype;
    storage : datatype;
    return : datatype;
    code : 'exp;
  }

type location = {
    loc_file : string;
    loc_pos : ((int * int) * (int * int)) option;
  }

type error = { err_loc: location; err_msg: string }

exception LiquidError of error



type primitive =
   (* resolved in LiquidCheck *)
  | Prim_unknown
  | Prim_coll_find
  | Prim_coll_update
  | Prim_coll_mem
  | Prim_coll_reduce
  | Prim_coll_map
  | Prim_coll_size

  (* generated in LiquidCheck *)
  | Prim_unused

  (* primitives *)
  | Prim_tuple_get_last
  | Prim_tuple_get
  | Prim_tuple_set_last
  | Prim_tuple_set
  | Prim_tuple

  | Prim_fail
  | Prim_self
  | Prim_balance
  | Prim_now
  | Prim_amount
  | Prim_gas
  | Prim_Left
  | Prim_Right
  | Prim_Source
  | Prim_eq
  | Prim_neq
  | Prim_lt
  | Prim_le
  | Prim_gt
  | Prim_ge
  | Prim_compare
  | Prim_add
  | Prim_sub
  | Prim_mul
  | Prim_ediv

  | Prim_map_find
  | Prim_map_update
  | Prim_map_mem
  | Prim_map_reduce
  | Prim_map_map
  | Prim_map_size

  | Prim_set_update
  | Prim_set_mem
  | Prim_set_reduce
  | Prim_set_size
  | Prim_set_map

  | Prim_Some
  | Prim_concat

  | Prim_list_reduce
  | Prim_list_map
  | Prim_list_size
  | Prim_list_rev

  | Prim_manager
  | Prim_create_account
  | Prim_create_contract
  | Prim_hash
  | Prim_hash_key
  | Prim_check
  | Prim_default_account


  | Prim_Cons
  | Prim_or
  | Prim_and
  | Prim_xor
  | Prim_not
  | Prim_abs
  | Prim_int
  | Prim_neg
  | Prim_lsr
  | Prim_lsl

  | Prim_exec


let primitive_of_string = Hashtbl.create 101
let string_of_primitive = Hashtbl.create 101


(* Some primitives should be kept internal:
* get and set
* get_last and set_last
* tuple
*)
let () =
  List.iter (fun (n,p) ->
      Hashtbl.add primitive_of_string n p;
      Hashtbl.add string_of_primitive p n;
    )
            [
              "get", Prim_tuple_get;
              "get_last", Prim_tuple_get_last;
              "set_last", Prim_tuple_set_last;
              "set", Prim_tuple_set;
              "tuple", Prim_tuple;

              "Array.get", Prim_tuple_get;
              "Array.set", Prim_tuple_set;

              "Current.fail", Prim_fail;
              "Current.contract", Prim_self;
              "Current.balance", Prim_balance;
              "Current.time", Prim_now;
              "Current.amount", Prim_amount;
              "Current.gas", Prim_gas;

              "Left", Prim_Left;
              "Right", Prim_Right;
              "Source", Prim_Source;
              "=", Prim_eq;
              "<>", Prim_neq;
              "<", Prim_lt;
              "<=", Prim_le;
              ">", Prim_gt;
              ">=", Prim_ge;
              "compare", Prim_compare;
              "+", Prim_add;
              "-", Prim_sub;
              "*", Prim_mul;
              "/", Prim_ediv;

              "Map.find", Prim_map_find;
              "Map.update", Prim_map_update;
              "Map.mem", Prim_map_mem;
              "Map.reduce", Prim_map_reduce;
              "Map.map", Prim_map_map;
              "Map.size", Prim_map_size;

              "Set.update", Prim_set_update;
              "Set.mem", Prim_set_mem;
              "Set.reduce", Prim_set_reduce;
              "Set.map", Prim_set_map;
              "Set.size", Prim_set_size;

              "Some", Prim_Some;
              "@", Prim_concat;

              "List.reduce", Prim_list_reduce;
              "List.map", Prim_list_map;
              "List.rev", Prim_list_rev;
              "List.size", Prim_list_size;

              "Contract.manager", Prim_manager;
              "Contract.create", Prim_create_contract;

              "Account.create", Prim_create_account;
              "Account.default", Prim_default_account;

              "Crypto.hash", Prim_hash;
              "Crypto.hash_key", Prim_hash_key;
              "Crypto.check", Prim_check;

              "::", Prim_Cons;
              "or", Prim_or;
              "&", Prim_and;
              "xor", Prim_xor;
              "not", Prim_not;
              "abs", Prim_abs;
              "int", Prim_int;
              ">>", Prim_lsr;
              "<<", Prim_lsl;

              "Lambda.pipe" , Prim_exec;
              "|>", Prim_exec;

              "Coll.update", Prim_coll_update;
              "Coll.mem", Prim_coll_mem;
              "Coll.find", Prim_coll_find;
              "Coll.map", Prim_coll_map;
              "Coll.reduce", Prim_coll_reduce;
              "Coll.size",Prim_coll_size;

              "<unknown>", Prim_unknown;
              "<unused>", Prim_unused;

            ]

let primitive_of_string s =
  try
    Hashtbl.find primitive_of_string s
  with Not_found ->
    Printf.eprintf "Debug: primitive_of_string(%S) raised Not_found\n%!" s;
    raise Not_found

let string_of_primitive prim =
  try
    Hashtbl.find string_of_primitive prim
  with Not_found ->
    Printf.eprintf "Debug: string_of_primitive(%d) raised Not_found\n%!"
                   (Obj.magic prim : int);
    raise Not_found


(* `variant` is the only parameterized type authorized in Liquidity.
   Its constructors, `Left` and `Right` must be constrained with type
   annotations, for the correct types to be propagated in the sources.
*)
type constructor =
  Constr of string
| Left of datatype
| Right of datatype
| Source of datatype * datatype

type pattern =
  | CConstr of string * string list
  | CAny

type 'ty exp = {
    desc : 'ty exp_desc;
    ty : 'ty;
    bv : StringSet.t;
    fail : bool;
    transfer : bool;
  }

and 'ty exp_desc =
  | Let of string * location * 'ty exp * 'ty exp
  | Var of string * location * string list
  | SetVar of string * location * string list * 'ty exp
  | Const of datatype * const
  | Apply of primitive * location * 'ty exp list
  | If of 'ty exp * 'ty exp * 'ty exp
  | Seq of 'ty exp * 'ty exp
  | LetTransfer of (* storage *) string * (* result *) string
                                 * location
                   * (* contract_ *) 'ty exp
                   * (* tez_ *) 'ty exp
                   * (* storage_ *) 'ty exp
                   * (* arg_ *) 'ty exp
                   * 'ty exp (* body *)
  | MatchOption of 'ty exp  (* argument *)
                     * location
                     * 'ty exp  (* ifnone *)
                     * string * 'ty exp (*  ifsome *)
  | MatchList of 'ty exp  (* argument *)
                 * location
                 * string * string * 'ty exp * (* ifcons *)
                       'ty exp (*  ifnil *)
  | Loop of string * location
              * 'ty exp  (* body *)
              * 'ty exp (*  arg *)

  | Lambda of string (* argument name *)
              * datatype (* argument type *)
              * location
              * 'ty exp (* body *)
              * datatype (* final datatype,
                            inferred during typechecking *)

  | Closure of string (* argument name *)
              * datatype (* argument type *)
              * location
              * (string * 'ty exp) list (* call environment *)
              * 'ty exp (* body *)
              * datatype (* final datatype,
                            inferred during typechecking *)

  | Record of location * (string * 'ty exp) list
  | Constructor of location * constructor * 'ty exp

  | MatchVariant of 'ty exp
                    * location
                    * (pattern * 'ty exp) list

  | MatchNat of 'ty exp  (* argument *)
                * location
                * string * 'ty exp (* ifplus *)
                * string * 'ty exp (* ifminus *)

type syntax_exp = unit exp
type typed_exp = datatype exp
type live_exp = (datatype * datatype StringMap.t) exp


let mk =
  let bv = StringSet.empty in
  fun desc ty ->
    let fail, transfer = match desc with
      | Const (_, _)
      | Var (_, _, _) -> false, false

      | LetTransfer _ -> true, true

      | SetVar (_, _, _, e)
      | Constructor (_, _, e)
      | Lambda (_, _, _, e, _) -> e.fail, e.transfer

      | Seq (e1, e2)
      | Let (_, _, e1, e2)
      | Loop (_, _, e1, e2) -> e1.fail || e2.fail, e1.transfer || e2.transfer

      | If (e1, e2, e3)
      | MatchOption (e1, _, e2, _, e3)
      | MatchNat (e1, _, _, e2, _, e3)
      | MatchList (e1, _, _, _, e2, e3) ->
        e1.fail || e2.fail || e3.fail,
        e1.transfer || e2.transfer || e3.transfer

      | Apply (prim, _, l) ->
        prim = Prim_fail || List.exists (fun e -> e.fail) l,
        List.exists (fun e -> e.transfer) l

      | Closure (_, _, _, env, e, _) ->
        e.fail || List.exists (fun (_, e) -> e.fail) env,
        e.transfer || List.exists (fun (_, e) -> e.transfer) env

      | Record (_, labels) ->
        List.exists (fun (_, e) -> e.fail) labels,
        List.exists (fun (_, e) -> e.transfer) labels

      | MatchVariant (e, _, cases) ->
        e.fail || List.exists (fun (_, e) -> e.fail) cases,
        e.transfer || List.exists (fun (_, e) -> e.transfer) cases
    in
    { desc; ty; bv; fail; transfer }


type michelson_exp =
  | M_INS of string
  | M_INS_CST of string * datatype * const
  | M_INS_EXP of string * datatype list * michelson_exp list

type 'a pre_michelson =
  | SEQ of 'a list
  | DIP of int * 'a
  | IF of 'a * 'a
  | IF_NONE of 'a * 'a
  | IF_CONS of 'a * 'a
  | IF_LEFT of 'a * 'a
  | LOOP of 'a

  | LAMBDA of datatype * datatype * 'a
  | EXEC

  | DUP of int
  | DIP_DROP of int * int
  | DROP
  | CAR
  | CDR
  | CDAR of int
  | CDDR of int
  | PUSH of datatype * const
  | PAIR
  | COMPARE
  | LE | LT | GE | GT | NEQ | EQ
  | FAIL
  | NOW
  | TRANSFER_TOKENS
  | ADD
  | SUB
  | BALANCE
  | SWAP
  | GET
  | UPDATE
  | SOME
  | CONCAT
  | MEM
  | MAP
  | REDUCE

  | SELF
  | AMOUNT
  | STEPS_TO_QUOTA
  | MANAGER
  | CREATE_ACCOUNT
  | CREATE_CONTRACT
  | H
  | HASH_KEY
  | CHECK_SIGNATURE

  | CONS
  | OR
  | XOR
  | AND
  | NOT

  | INT
  | ABS
  | NEG
  | MUL

  | LEFT of datatype
  | RIGHT of datatype

  | EDIV
  | LSL
  | LSR

  | SOURCE of datatype * datatype

  | SIZE
  | DEFAULT_ACCOUNT

  (* obsolete *)
  | MOD
  | DIV

type noloc_michelson = { i : noloc_michelson pre_michelson }

type loc_michelson = {
  loc : location;
  ins : loc_michelson pre_michelson;
}

let mic ins = ins
let mic_loc loc ins = { loc; ins }

type closure_env = {
  env_vars :  (string (* name outside closure *)
               * datatype
               * int (* index *)
               * (int ref * (* usage counter inside closure *)
                  int ref (* usage counter outside closure *)
                 )) StringMap.t;
  env_bindings : (typed_exp (* expression to access variable inside closure *)
                  * (int ref * (* usage counter inside closure *)
                     int ref (* usage counter outside closure *)
                    )) StringMap.t;
  call_bindings : (string * typed_exp) list;
}

type env = {
    (* name of file being compiled *)
    filename : string;

    (* fields modified in LiquidFromOCaml *)
    (* type definitions *)
    mutable types : datatype StringMap.t;
    (* labels of records in type definitions *)
    mutable labels : (string * int * datatype) StringMap.t;
    (* constructors of sum-types in type definitions *)
    mutable constrs : (string * datatype) StringMap.t;
  }

(* fields updated in LiquidCheck *)
type 'a typecheck_env = {
    warnings : bool;
    counter : int ref;
    vars : (string * datatype * int ref) StringMap.t;
    env : env;
    to_inline : datatype exp StringMap.t ref;
    contract : 'a contract;
    clos_env : closure_env option;
}

(* decompilation *)

type node = {
  num : int;
  loc : location;
  mutable kind : node_kind;
  mutable args : node list; (* dependencies *)

  mutable next : node option;
  mutable prevs : node list;
}

 and node_kind =
   | N_UNKNOWN of string
   | N_VAR of string
   | N_START
   | N_IF of node * node
   | N_IF_RESULT of node * int
   | N_IF_THEN of node
   | N_IF_ELSE of node
   | N_IF_END of node * node
   | N_IF_END_RESULT of node * node option * int
   | N_IF_NONE of node
   | N_IF_SOME of node * node
   | N_IF_NIL of node
   | N_IF_CONS of node * node * node
   | N_IF_LEFT of node * node
   | N_IF_RIGHT of node * node
   | N_IF_PLUS of node * node
   | N_IF_MINUS of node * node
   | N_TRANSFER of node * node
   | N_TRANSFER_RESULT of int
   | N_CONST of datatype * const
   | N_PRIM of string
   | N_FAIL
   | N_LOOP of node * node
   | N_LOOP_BEGIN of node
   | N_LOOP_ARG of node * int
   | N_LOOP_RESULT of (* N_LOOP *) node
                                   * (* N_LOOP_BEGIN *) node * int
   | N_LOOP_END of (* N_LOOP *) node
                                * (* N_LOOP_BEGIN *) node
                                * (* final_cond *) node
   | N_LAMBDA of node * node * datatype * datatype
   | N_LAMBDA_BEGIN
   | N_LAMBDA_END of node
   | N_END
   | N_LEFT of datatype
   | N_RIGHT of datatype
   | N_SOURCE of datatype * datatype
   | N_ABS

type node_exp = node * node

type warning =
  | Unused of string
  | UnusedMatched of string
