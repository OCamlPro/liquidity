[%%version 2.0]

type accounts = (address, nat) big_map

type metadata = {
  version : nat * nat; (* version of token standard *)
  decimals : nat;
  name : string;
  symbol : string;
}

(* Implementation dependent: can be any type *)
type internal_storage = {
  owner : address;
  rest : unit;
}

type storage = {
  accounts : accounts;
  totalSupply : nat;
  metadata : metadata;
  internal : internal_storage;
}

contract type TokenReceiver = sig
  val%entry receiveTokens : address * nat * bytes option
end

let%init storage owner decimals name symbol =
  {
    accounts = BigMap;
    totalSupply = 0p;
    metadata = {
      version = 1p, 2p; (* Version of standard : 1.2 *)
      decimals;
      name;
      symbol;
    };
    internal = {
      owner;
      rest = ();
    }
  }

(* Helper functions *)

let[@inline] no_base_currency () =
  if Current.amount () <> 0DUN then failwith "Don't send DUN"

let perform_transfer (dest, tokens, (accounts : accounts)) =
  (* Helper functions *)
  let[@inline] get_balance (a, (accounts : accounts)) =
    match Map.find a accounts with
    | None -> 0p
    | Some balance -> balance
  in
  let[@inline] set_balance (a, balance, (accounts : accounts)) =
    let v = if balance = 0p then None else Some balance in
    Map.update a v accounts
  in
  (* Decrease balance of sender *)
  let sender_balance = get_balance (Current.sender (), accounts) in
  let new_sender_balance = match is_nat (sender_balance - tokens) with
    | None -> failwith ("Not enough tokens for transfer", sender_balance)
    | Some b -> b in
  let accounts =
    set_balance (Current.sender (), new_sender_balance, accounts) in
  (* Increase balance of destination *)
  let dest_balance = get_balance (dest, accounts) in
  let new_dest_balance = dest_balance + tokens in
  let accounts = set_balance (dest, new_dest_balance, accounts) in
  accounts

let[@inline] get_balance (a, (accounts : accounts)) =
  match Map.find a accounts with
  | None -> 0p
  | Some balance -> balance

let[@inline] set_balance (a, balance, (accounts : accounts)) =
    let v = if balance = 0p then None else Some balance in
    Map.update a v accounts

(*------------------ Transfer tokens --------------------*)

let%entry transfer (dest, tokens, data) storage =
  no_base_currency ();
  let accounts = perform_transfer (dest, tokens, storage.accounts) in
  let ops =
    match [%handle TokenReceiver.receiveTokens] dest
    (* Alternative syntax: *)
    (* [%handle: val%entry receiveTokens : address * nat * bytes option] dest *)
    with
    | Some dest ->
      [dest.receiveTokens (Current.sender (), tokens, data) ~amount:0DUN]
    | None -> match data with
      | None -> []
      | Some _ -> failwith "Cannot send data to a non TokenReceiver contract"
  in
  ops, storage.accounts <- accounts


(* --------------- Storage access from outside ---------------- *)

let%entry balanceOf (addr, (balanceOf_handler : [%handle: address * nat])) storage =
  no_base_currency ();
  let balance = get_balance (addr, storage.accounts) in
  [ Contract.call balanceOf_handler (addr, balance) ~amount:0DUN ], storage

let%entry totalSupply (totalSupply_handler : [%handle: nat]) storage =
  no_base_currency ();
  [ Contract.call totalSupply_handler storage.totalSupply ~amount:0DUN ], storage

(* ------------------ Burning tokens  ------------------------ *)

let%entry burn (tokens : nat) storage =
  no_base_currency ();
  let accounts = storage.accounts in
  (* Decrease balance of sender *)
  let sender_balance = get_balance (Current.sender (), accounts) in
  let new_sender_balance = match is_nat (sender_balance - tokens) with
    | None ->
      failwith ("Not enough tokens for transfer", sender_balance)
    | Some b -> b in
  let accounts =
    set_balance (Current.sender (), new_sender_balance, accounts) in
  let totalSupply = match is_nat (storage.totalSupply - tokens) with
    | None -> failwith ()
    | Some t -> t in
  [], (storage.accounts <- accounts).totalSupply <- totalSupply


(* ------------------ Minting tokens ------------------------ *)
(* Not part of standard *)

let%entry mint new_accounts storage =
  no_base_currency ();
  if Current.sender () <> storage.internal.owner then
    failwith "Only owner can create accounts";
  let accounts, totalSupply =
    List.fold (fun ((dest, tokens), acc) ->
        let accounts, totalSupply = acc in
        if tokens = 0p then acc
        else
          let balance = get_balance (dest, accounts) in
          (Map.add dest (balance + tokens) accounts,
           totalSupply + tokens)
      ) new_accounts (storage.accounts, storage.totalSupply) in
  [], (storage.accounts <- accounts).totalSupply <- totalSupply
