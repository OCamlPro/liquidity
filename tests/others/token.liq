
type account = {
  balance : nat;
  allowances : (address, nat) map;
}

type storage = {
  accounts : (address, account) big_map;
  version : nat; (* version of token standard *)
  totalSupply : nat;
  decimals : nat;
  name : string;
  symbol : string;
  owner : address;
  conversion_rate : nat * dun;
}

let base_fee = 0.041_DUN
let base_burn = 8p

let%init storage owner totalSupply decimals name symbol conversion_rate =
  if conversion_rate.(1) = 0_DUN then failwith "Bad conversion rate";
  let owner_account  =
    { balance = totalSupply;
      allowances = Map } in
  let accounts =
    Map.add owner owner_account BigMap in
  {
    accounts;
    version = 1p;
    totalSupply;
    decimals;
    name;
    symbol;
    owner;
    conversion_rate;
  }

let get_account (a, (accounts : (address, account) big_map)) =
  match Map.find a accounts with
  | None -> { balance = 0p; allowances = Map }
  | Some account -> account

let perform_transfer
    (from, dest, tokens, storage) =
  let accounts = storage.accounts in
  let account_sender = get_account (from, accounts) in
  let new_account_sender = match is_nat (account_sender.balance - tokens) with
    | None ->
      failwith ("Not enough tokens for transfer", account_sender.balance)
    | Some b -> account_sender.balance <- b in
  let accounts = Map.add from new_account_sender accounts in
  let account_dest = get_account (dest, accounts) in
  let new_account_dest =
    account_dest.balance <- account_dest.balance + tokens in
  let accounts = Map.add dest new_account_dest accounts in
  [], storage.accounts <- accounts


let take_fee_as_tokens (sender, (fee, burn), storage) =
  let (c_tok, c_dun) = storage.conversion_rate in
  let tokens = (fee + burn * 0.001_DUN) * c_tok / c_dun in
  match tokens with
  | None -> failwith ()
  | Some (fee_tokens, _) ->
    let accounts = storage.accounts in
    let account_sender = get_account (sender, accounts) in
    let new_account_sender = match is_nat (account_sender.balance - fee_tokens) with
      | None ->
        failwith ("Not enough tokens to pay fees", account_sender.balance, fee_tokens)
      | Some b -> account_sender.balance <- b in
    let accounts = Map.add sender new_account_sender accounts in
    storage.accounts <- accounts

let fee_transfer dest storage =
  let burn = if Map.mem dest storage.accounts then 0p else base_burn in
  base_fee, burn

let%entry transfer (dest, tokens) storage = begin[@fee
  fee_transfer dest storage
]
  let storage =
    if Current.collect_call () then
      take_fee_as_tokens
        (Current.sender (), fee_transfer dest storage, storage)
    else storage in
  perform_transfer (Current.sender (), dest, tokens, storage)
end

let fee_approve = base_fee, base_burn + 20p

let%entry approve (spender, tokens) storage = begin[@fee
  fee_approve
]
  let storage =
    if Current.collect_call () then
      take_fee_as_tokens
        (Current.sender (), fee_approve, storage)
    else storage in
  let account_sender = get_account (Current.sender (), storage.accounts) in
  let account_sender =
    account_sender.allowances <-
      if tokens = 0p then
        Map.remove spender account_sender.allowances
      else
        Map.add spender tokens account_sender.allowances in
  let storage = storage.accounts <-
      Map.add (Current.sender ()) account_sender storage.accounts in
  [], storage
end

let fee_transferFrom dest storage =
  let burn = if Map.mem dest storage.accounts then 0p else base_burn in
  base_fee + 0.002_DUN, burn

let%entry transferFrom (from, dest, tokens) storage = begin[@fee
  fee_transferFrom dest storage
]
  let storage =
    if Current.collect_call () then
      take_fee_as_tokens
        (Current.sender (), fee_transferFrom dest storage, storage)
    else storage in
  let account_from = get_account (from, storage.accounts) in
  let new_allowances_from =
    match Map.find (Current.sender ()) account_from.allowances with
    | None -> failwith ("Not allowed to spend from", from)
    | Some allowed ->
      match is_nat (allowed - tokens) with
      | None ->
        failwith ("Not enough allowance for transfer", allowed)
      | Some allowed ->
        if allowed = 0p then
          Map.remove (Current.sender ()) account_from.allowances
        else
          Map.add (Current.sender ()) allowed account_from.allowances in
  let account_from = account_from.allowances <- new_allowances_from in
  let storage = storage.accounts <-
      Map.add from account_from storage.accounts in
  perform_transfer (from, dest, tokens, storage)
end


(* ------------- Storage access from outside ------------- *)

contract type NatContract = sig
  type storage
  val%entry main : nat -> _
end

contract type NatNatContract = sig
  type storage
  val%entry main : nat * nat -> _
end

let%entry balanceOf (spender, forward) storage = begin[@fee
   failwith "I don't pay fees for balanceOf"
]
  let spender_balance = match Map.find spender storage.accounts with
    | None -> 0p
    | Some account -> account.balance in
  [ forward.main spender_balance ~amount:0tz ], storage
end

let%entry allowance (from, spender, forward) storage = begin[@fee
  failwith "I don't pay fees for allowance"
]
  let spender_allowance = match Map.find from storage.accounts with
    | None -> 0p, 0p
    | Some account -> match Map.find spender account.allowances with
      | None -> 0p, account.balance
      | Some allowed -> allowed, account.balance in
  [ forward.main spender_allowance ~amount:0tz ], storage
end

(* -------------- Creating accounts ------------------------ *)

(*
Fee to the baker: đ0.021479
Consumed gas: 211548
*)
let%entry createAccount (dest, tokens) storage = begin[@fee
   failwith "I don't pay fees for createAccount"
]
  if Current.sender () <> storage.owner then
    failwith "Only owner can create accounts";
  perform_transfer (storage.owner, dest, tokens, storage)
end

let%entry createAccounts new_accounts storage = begin[@fee
   failwith "I don't pay fees for createAccounts"
]
  if Current.sender () <> storage.owner then
    failwith "Only owner can create accounts";
  List.fold (fun ((dest, tokens), (_ops, storage)) ->
      perform_transfer (storage.owner, dest, tokens, storage)
    ) new_accounts ([], storage)
end
