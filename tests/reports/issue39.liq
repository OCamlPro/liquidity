[%%version 0.12]

type token_p = 
| Token_info
| Balance of key_hash
| Transfer_token of (key_hash * nat)
| Approve of (key_hash * nat)
| Transfer_token_from_approve_dict of (key_hash * key_hash * nat)

type token_r =
| General_return of nat
| Token_info_return of nat * nat * string * string
| Balance_return of nat

type token_contract = (token_p, token_r) contract

type token = nat
type odd_index = nat
type odd_decimal = nat    (* it should be div 1000 *)

type state =
| Open
| Disabled of string
| Settled

type info = {
  name : string;
  created_date : timestamp;
  bet_time_range : timestamp * timestamp;
  report_time_range : timestamp * timestamp;
  odds_lst : string list;
}

type bookmaker = {
  key : key_hash option;
  margin : token;
  odds : (odd_index, odd_decimal) map;
}

type storage_t = {
  self_key : key_hash;
  token_contract : token_contract;

  state : state;
  info : info;
  bookmaker : bookmaker;

  all_token_amount : token;
  all_bets_token_amount: (odd_index, token) map;
  orders : (odd_index, (key_hash * odd_decimal * token) list) map;
  reports : (key_hash, odd_index) map;
  distribution: (key_hash, token) map;
}

type bet_parameter_t = 
| Check
| Override of (key_hash * token) option * storage_t

type bet_return_t = nat * storage_t

type mod_parameter_t = key_hash * (token * odd_index * odd_decimal) * storage_t * (bet_parameter_t, bet_return_t) contract

type return_t = 
| Success
| Transfer_fail of nat
| Fail of string

type parameter_t = (token * odd_index * odd_decimal) * (mod_parameter_t, return_t) contract * (bet_parameter_t, bet_return_t) contract

let%entry main (parameter : parameter_t) (storage : parameter_t): return_t * parameter_t =
  (* let bet_contract = parameter.(2) in
  let (result, parameter) = Contract.call bet_contract 0tz parameter Check in *)

  (* let ((token, odd_index, odd_decimal), _, _) = parameter in *)

  let odds_reduce (x : ((odd_index * token) * token)) =
    (* let curr_odd_index = x.(0).(0) in
    let token = x.(0).(1) in *)
    (* let acc_token = x.(1) in *)
    (* if curr_odd_index <> 1p then
      acc_token + token
    else *)
    0p
  in
  (* let bet_storage = result.(1) in
  let bet_tokens = Map.reduce odds_reduce bet_storage.all_bets_token_amount 0p in *)
  (* let all_margin = bet_storage.bookmaker.margin + bet_tokens in 
  let orders_reduce (x : ((key_hash * odd_decimal * token) * token)) =
    let odd_decimal = x.(0).(1) in
    let token = x.(0).(2) in
    let acc_token = x.(1) in
    let result = match token * odd_decimal / 1000p with
      | None -> 0p
      | Some x -> x.(0)
    in 
    acc_token + result
  in *)
  (* let bet_lst = 
    let source = Contract.manager (Source : (unit, unit) contract) in
    match Map.find odd_index bet_storage.orders with
    | None -> [(source, odd_decimal, token)]
    | Some x -> (source, odd_decimal, token) :: x
  in  
  let counter_margin = List.reduce orders_reduce bet_lst 0p in  *)       
  Current.failwith ()
  (* if all_margin >= counter_margin then
    let source = Contract.manager (Source : (unit, unit) contract) in
    let arg = (source, parameter.(0), bet_storage, parameter.(2)) in
    let (result, parameter) = Contract.call parameter.(1) 0tz parameter arg in
    (result, parameter)
  else
    (Fail "Insufficient margin", parameter) *)

(*
with arg

(Pair (Pair 0 (Pair 0 0)) (Pair "TZ1nk13rZbQ6i1pns7C3V3qpoAfXWEryheLT" "TZ1kjNxXUqvJKRKWFNZtgSCUiPr2GdRkA7cp"))
*)

