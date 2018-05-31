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
module IntMap = Map.Make(struct type t = int let compare = compare end)

exception InvalidFormat of string * string

type tez = { tezzies : string; mutez : string option }
type integer = { integer : Z.t }

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
  | CBigMap of (const * const) list
  | CList of const list
  | CSet of const list

  | CLeft of const
  | CRight of const

  | CKey_hash of string
  | CContract of string

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

  | Toption of datatype
  | Tlist of datatype
  | Tset of datatype

  | Tmap of datatype * datatype
  | Tbigmap of datatype * datatype
  | Tcontract of datatype * datatype
  | Tor of datatype * datatype
  | Tlambda of datatype * datatype

  (* liquidity extensions *)
  | Trecord of string * (string * datatype) list
  | Tsum of string * (string * datatype) list
  | Tclosure of (datatype * datatype) * datatype
  | Tfail

let size_of_type = function
  | Ttuple l -> List.length l
  | Trecord (_, l) -> List.length l
  | _ -> raise (Invalid_argument "size_of_type")

let comparable_type = function
  | Tbool
  | Tint
  | Tnat
  | Ttez
  | Tstring
  | Ttimestamp
  | Tkey_hash -> true
  | _ -> false


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
  | Prim_tuple_get
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
  | Prim_map_add
  | Prim_map_remove
  | Prim_map_mem
  | Prim_map_reduce
  | Prim_map_map
  | Prim_map_size

  | Prim_set_update
  | Prim_set_add
  | Prim_set_remove
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


type prim_fold =
  | Prim_map_iter
  | Prim_set_iter
  | Prim_list_iter
  | Prim_map_fold
  | Prim_set_fold
  | Prim_list_fold

  | Prim_coll_iter
  | Prim_coll_fold


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
              "~-", Prim_neg;

              "Map.find", Prim_map_find;
              "Map.update", Prim_map_update;
              "Map.add", Prim_map_add;
              "Map.remove", Prim_map_remove;
              "Map.mem", Prim_map_mem;
              "Map.reduce", Prim_map_reduce;
              "Map.map", Prim_map_map;
              "Map.size", Prim_map_size;

              "Set.update", Prim_set_update;
              "Set.add", Prim_set_add;
              "Set.remove", Prim_set_remove;
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
              "lor", Prim_or;
              "or", Prim_or;
              "||", Prim_or;
              "&", Prim_and;
              "land", Prim_and;
              "&&", Prim_and;
              "lxor", Prim_xor;
              "xor", Prim_xor;
              "not", Prim_not;
              "abs", Prim_abs;
              "int", Prim_int;
              ">>", Prim_lsr;
              "lsr", Prim_lsr;
              "<<", Prim_lsl;
              "lsl", Prim_lsl;

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


let fold_primitive_of_string = Hashtbl.create 8
let string_of_fold_primitive = Hashtbl.create 8
let () =
  List.iter (fun (n,p) ->
      Hashtbl.add fold_primitive_of_string n p;
      Hashtbl.add string_of_fold_primitive p n;
    )
            [
              "Map.iter", Prim_map_iter;
              "Set.iter", Prim_set_iter;
              "List.iter", Prim_list_iter;
              "Map.fold", Prim_map_fold;
              "Set.fold", Prim_set_fold;
              "List.fold", Prim_list_fold;
              "Coll.iter", Prim_coll_iter;
              "Coll.fold", Prim_coll_fold;
            ]

let fold_primitive_of_string s =
  try
    Hashtbl.find fold_primitive_of_string s
  with Not_found ->
    Printf.eprintf "Debug: fold_primitive_of_string(%S) raised Not_found\n%!" s;
    raise Not_found

let string_of_fold_primitive prim =
  try
    Hashtbl.find string_of_fold_primitive prim
  with Not_found ->
    Printf.eprintf "Debug: string_of_fold_primitive(%d) raised Not_found\n%!"
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

type ('ty, 'a) exp = {
  desc : ('ty, 'a) exp_desc;
  name : string option;
  ty : 'ty;
  bv : StringSet.t;
  fail : bool;
  transfer : bool;
}

and ('ty, 'a) exp_desc =
  | Let of string * location * ('ty, 'a) exp * ('ty, 'a) exp
  | Var of string * location * string list
  | SetVar of string * location * string list * ('ty, 'a) exp
  | Const of location * datatype * const
  | Apply of primitive * location * ('ty, 'a) exp list
  | If of ('ty, 'a) exp * ('ty, 'a) exp * ('ty, 'a) exp
  | Seq of ('ty, 'a) exp * ('ty, 'a) exp
  | LetTransfer of (* storage *) string * (* result *) string
                                 * location
                   * (* contract_ *) ('ty, 'a) exp
                   * (* tez_ *) ('ty, 'a) exp
                   * (* storage_ *) ('ty, 'a) exp
                   * (* arg_ *) ('ty, 'a) exp
                   * ('ty, 'a) exp (* body *)
  | MatchOption of ('ty, 'a) exp  (* argument *)
                     * location
                     * ('ty, 'a) exp  (* ifnone *)
                     * string * ('ty, 'a) exp (*  ifsome *)
  | MatchList of ('ty, 'a) exp  (* argument *)
                 * location
                 * string * string * ('ty, 'a) exp * (* ifcons *)
                       ('ty, 'a) exp (*  ifnil *)
  | Loop of string * location
              * ('ty, 'a) exp  (* body *)
              * ('ty, 'a) exp (*  arg *)

  | Fold of prim_fold (* iter/fold *)
              * string * location
              * ('ty, 'a) exp (* body *)
              * ('ty, 'a) exp (* arg *)
              * ('ty, 'a) exp (* acc *)

  | Lambda of string (* argument name *)
              * datatype (* argument type *)
              * location
              * ('ty, 'a) exp (* body *)
              * datatype (* final datatype,
                            inferred during typechecking *)

  | Closure of string (* argument name *)
              * datatype (* argument type *)
              * location
              * (string * ('ty, 'a) exp) list (* call environment *)
              * ('ty, 'a) exp (* body *)
              * datatype (* final datatype,
                            inferred during typechecking *)

  | Record of location * (string * ('ty, 'a) exp) list
  | Constructor of location * constructor * ('ty, 'a) exp

  | MatchVariant of ('ty, 'a) exp
                    * location
                    * (pattern * ('ty, 'a) exp) list

  | MatchNat of ('ty, 'a) exp  (* argument *)
                * location
                * string * ('ty, 'a) exp (* ifplus *)
                * string * ('ty, 'a) exp (* ifminus *)

  | Failwith of string * location

type typed
type encoded
type syntax_exp = (unit, unit) exp
type typed_exp = (datatype, typed) exp
type encoded_exp = (datatype, encoded) exp
type live_exp = (datatype * datatype StringMap.t, encoded) exp


let mk =
  let bv = StringSet.empty in
  fun ?name desc ty ->
    let fail, transfer = match desc with
      | Const (_, _, _)
      | Var (_, _, _) -> false, false

      | Failwith _ -> true, false

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
      | MatchList (e1, _, _, _, e2, e3)
      | Fold (_, _, _, e1, e2, e3) ->
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
    { desc; name; ty; bv; fail; transfer }


type michelson_exp =
  | M_INS of string * string option
  | M_INS_ANNOT of string
  | M_INS_CST of string * datatype * const * string option
  | M_INS_EXP of string * datatype list * michelson_exp list * string option

type 'a pre_michelson =
  | ANNOT of string
  | SEQ of 'a list
  | DIP of int * 'a
  | IF of 'a * 'a
  | IF_NONE of 'a * 'a
  | IF_CONS of 'a * 'a
  | IF_LEFT of 'a * 'a
  | LOOP of 'a
  | ITER of 'a

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
  | FAIL of string option
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

type loc_michelson = {
  loc : location;
  ins : loc_michelson pre_michelson;
  mutable loc_name : string option;
}

(* let mic ins = ins *)
(* let mic_loc loc ins = { loc; ins } *)

type closure_env = {
  env_vars :  (string (* name outside closure *)
               * datatype
               * int (* index *)
               * (int ref * (* usage counter inside closure *)
                  int ref (* usage counter outside closure *)
                 )) StringMap.t;
  env_bindings : (encoded_exp (* expression to access variable inside closure *)
                  * (int ref * (* usage counter inside closure *)
                     int ref (* usage counter outside closure *)
                    )) StringMap.t;
  call_bindings : (string * encoded_exp) list;
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
    annot : bool;
    counter : int ref;
    vars : (string * datatype * bool (* fails *) ) StringMap.t;
    vars_counts : int ref StringMap.t;
    env : env;
    to_inline : encoded_exp StringMap.t ref;
    contract : 'a contract;
    clos_env : closure_env option;
}

let empty_typecheck_env ~warnings contract env = {
  warnings;
  annot=false;
  counter = ref 0;
  vars = StringMap.empty;
  vars_counts = StringMap.empty;
  to_inline = ref StringMap.empty;
  env = env;
  clos_env = None;
  contract ;
}


let new_binding env name ?(fail=false) ty =
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (name, ty, fail) env.vars;
              vars_counts = StringMap.add name count env.vars_counts;
            } in
  (env, count)


(* decompilation *)

type node = {
  num : int;
  loc : location;
  mutable node_name : string option;
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
   | N_FAIL of string option
   | N_LOOP of node * node
   | N_LOOP_BEGIN of node
   | N_LOOP_ARG of node * int
   | N_LOOP_RESULT of (* N_LOOP *) node
                                   * (* N_LOOP_BEGIN *) node * int
   | N_LOOP_END of (* N_LOOP *) node
                                * (* N_LOOP_BEGIN *) node
                                * (* final_cond *) node
   | N_FOLD of node * node
   | N_FOLD_BEGIN of node
   | N_FOLD_ARG of node * int
   | N_FOLD_RESULT of node (* N_FOLD *)
                      * node * int (* N_FOLD_BEGIN *)
   | N_FOLD_END of node (* N_FOLD *)
                   * node (* N_FOLD_BEGIN *)
                   * node (* accumulator *)
   | N_LAMBDA of node * node * datatype * datatype
   | N_LAMBDA_BEGIN
   | N_LAMBDA_END of node
   | N_END
   | N_LEFT of datatype
   | N_RIGHT of datatype
   | N_SOURCE of datatype * datatype
   | N_ABS

type node_exp = node * node


type syntax_init = (
    (string * location * datatype) list (* arguments *)
    * syntax_exp (* init code *)
  )

type syntax_contract = syntax_exp contract
type typed_contract = typed_exp contract
type encoded_contract = encoded_exp contract
type michelson_contract = michelson_exp contract
type node_contract = node_exp contract
type loc_michelson_contract = loc_michelson contract

let noloc = { loc_file = "<unspecified>"; loc_pos = None }

let dummy_syntax_contract : syntax_contract = {
    parameter = Tunit;
    storage = Tunit;
    return = Tunit;
    code = mk (Const (noloc, Tunit, CUnit)) ();
  }

type warning =
  | Unused of string
  | UnusedMatched of string
