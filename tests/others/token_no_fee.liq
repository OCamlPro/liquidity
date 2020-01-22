
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

type fee = {
  tx_fee : nat ; (* tokens *)
  tx_burn : nat ; (* tokens *)
}


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

let perform_transfer from dest tokens storage =
  let get_account a (accounts : (address, account) big_map) =
    match Map.find a accounts with
    | None -> { balance = 0p; allowances = Map }
    | Some account -> account
  in
  let accounts = storage.accounts in
  let account_sender = get_account from accounts in
  let new_account_sender = match is_nat (account_sender.balance - tokens) with
    | None ->
      failwith ("Not enough tokens for transfer", account_sender.balance)
    | Some b -> account_sender.balance <- b in
  let accounts = Map.add from new_account_sender accounts in
  let account_dest = get_account dest accounts in
  let new_account_dest =
    account_dest.balance <- account_dest.balance + tokens in
  let accounts = Map.add dest new_account_dest accounts in
  ([] : operation list), storage.accounts <- accounts

let get_account a (accounts : (address, account) big_map) =
  match Map.find a accounts with
  | None -> { balance = 0p; allowances = Map }
  | Some account -> account

let precheck source tokens (accounts : (address, account) big_map) =
  match Map.find source accounts with
  | None -> failwith "Precheck: no account for source"
  | Some account ->
    if account.balance < tokens then
      failwith "Precheck: insufficient source balance"

let convert_token_fee (token_fee : fee) (conversion_rate : nat * dun) =
  let token_rate, dun_rate = conversion_rate in
  let dun_fee = match token_fee.tx_fee * dun_rate / token_rate with
    | None -> failwith ()
    | Some (fee, _change) -> fee in
  let dun_burn = match token_fee.tx_burn * dun_rate / token_rate with
    | None -> failwith ()
    | Some (burn, _change) -> burn in
  let burn_bytes = match dun_burn / 0.001_DUN (* = price of one byte *) with
    | None -> failwith ()
    | Some (nb_bytes, _) -> nb_bytes in
  (dun_fee, burn_bytes)

let[@inline] take_fee (fee, storage) =
  if Current.collect_call () then
    (perform_transfer
       (Current.sender ()) storage.owner
       (fee.tx_fee + fee.tx_burn) storage).(1)
  else storage

let%entry transfer (dest, tokens, fee) storage = begin[@fee
  precheck (Current.sender ()) (tokens + fee.tx_fee + fee.tx_burn) storage.accounts;
  convert_token_fee fee storage.conversion_rate
]
  let storage = take_fee (fee, storage) in
  perform_transfer (Current.sender ()) dest tokens storage
end

let%entry approve (spender, tokens, fee) storage = begin[@fee
  precheck (Current.sender ()) (fee.tx_fee + fee.tx_burn) storage.accounts;
  convert_token_fee fee storage.conversion_rate
]
  let storage = take_fee (fee, storage) in
  let account_sender = get_account (Current.sender ()) storage.accounts in
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

let%entry transferFrom (from, dest, tokens, fee) storage = begin[@fee
  precheck (Current.sender ()) (fee.tx_fee + fee.tx_burn) storage.accounts;
  precheck from tokens storage.accounts;
  convert_token_fee fee storage.conversion_rate
]
  let storage = take_fee (fee, storage) in
  let account_from = get_account from storage.accounts in
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
  perform_transfer from dest tokens storage
end


(* ------------- Storage access from outside ------------- *)

contract type NatContract = sig
  type storage
  val%entry default : nat
end

contract type NatNatContract = sig
  type storage
  val%entry default : nat * nat
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

let%entry createAccounts new_accounts storage = begin[@fee
   failwith "I don't pay fees for createAccounts"
]
  if Current.sender () <> storage.owner then
    failwith "Only owner can create accounts";
  List.fold (fun ((dest, tokens), (_ops, storage)) ->
      perform_transfer storage.owner dest tokens storage
    ) new_accounts ([], storage)
end

(* --------------------- Managing ------------------------ *)

let%entry updateConversionRate new_rate storage = begin[@fee
   failwith "I don't pay fees for updateConversionRate"
]
  if Current.sender () <> storage.owner then
    failwith "Only owner can change rate";
  [], storage.conversion_rate <- new_rate
end

let%entry default () storage = begin[@fee
   failwith "I don't pay fees for default"
]
  (* accept payments *)
  [], storage
end
