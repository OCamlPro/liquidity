(**************************************************************************)
(* Token contract with voting capabilities. Owner can propose votes, with *)
(* various actions depending on the result of the vote. Votes are tallied *)
(* by taking stakes owned into account.                                   *)
(**************************************************************************)

(** Type of an account stored in the big map *)
type account = {
  balance : nat; (** Balance in smallest unit of token *)
  allowances : (address, nat) map;
  (** Allowances, i.e. accounts athorized to spend from this account
      within given limits *)
}

(** Vote *)
type vote = {
  start_date : timestamp;
  end_date : timestamp;
  cast_votes : (address, string) map; (** Votes cast so far *)
  min_quorum : nat; (** Minimum number of stakes cast in total for the
                        vote to be takend into account (set to 0p for
                        no quorum ) *)
  actions : (string, unit -> operation list) map;
  (** Allowed votes mapped to actions *)
}

type storage = {
  accounts : (address, account) big_map;
  (** Big map of all accounts for this token *)
  version : nat; (** version of token standard *)
  totalSupply : nat; (** Total supply (set at deployment) *)
  decimals : nat; (** Decimal, to decide smallest unit *)
  name : string; (** Name of token *)
  symbol : string; (** Symbol used on exchanges and exploreres *)
  owner : address; (** Owner of token *)
  votes : (string, vote) map; (** Map of ongoing votes *)
}

let%init storage owner totalSupply decimals name symbol =
  let owner_account  =
    { balance = totalSupply;
      allowances = Map } in
  (* Only one account (owner) with whole supply when deploying *)
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
    votes = Map;
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


(** Transfer [tokens] from sender of transaction's account to [dest]'s
    account *)
let%entry transfer (dest, tokens) storage =
  perform_transfer (Current.sender (), dest, tokens, storage)

(** Approve [spender] to spend [tokens] from sender's account *)
let%entry approve (spender, tokens) storage =
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

(** Transfer from an account from which we are allowed to spend from *)
let%entry transferFrom (from, dest, tokens) storage =
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



(* ------------- Storage access from outside ------------- *)

contract type NatContract = sig
  type storage
  val%entry main : nat -> _
end

contract type NatNatContract = sig
  type storage
  val%entry main : nat * nat -> _
end

let%entry balanceOf (spender, forward) storage =
  let spender_balance = match Map.find spender storage.accounts with
    | None -> 0p
    | Some account -> account.balance in
  [ forward.main spender_balance ~amount:0tz ], storage

let%entry allowance (from, spender, forward)
    storage =
  let spender_allowance = match Map.find from storage.accounts with
    | None -> 0p, 0p
    | Some account -> match Map.find spender account.allowances with
      | None -> 0p, account.balance
      | Some allowed -> allowed, account.balance in
  [ forward.main spender_allowance ~amount:0tz ], storage


(* -------------- Creating accounts ------------------------ *)

let%entry createAccounts new_accounts storage =
  if Current.sender () <> storage.owner then
    failwith "Only owner can create accounts";
  List.fold (fun ((dest, tokens), (_ops, storage)) ->
      perform_transfer (storage.owner, dest, tokens, storage)
    ) new_accounts ([], storage)


(* ------------------------- Votes -------------------------- *)

(** Create a vote *)
let%entry createVote (name, start_date, end_date, min_quorum, actions) storage =
  if Current.sender () <> storage.owner then
    failwith "Only owner can create vote";
  if Map.mem name storage.votes then
    failwith "There already exists an ongoing vote with the same name";
  if start_date > end_date then
    failwith "Start date cannpt be after end date";
  if Map.cardinal actions = 0p then
    failwith "This vote has no choices";
  [], storage.votes <-
    Map.add name
      { start_date; end_date; cast_votes = Map;
        min_quorum; actions } storage.votes

(** Vote for a choice in an ongoing vote *)
let%entry vote (vote_name, vote_choice) storage =
  if Current.sender () = storage.owner then failwith "Owner cannot vote";
  let vote = match Map.find vote_name storage.votes with
    | None -> failwith "No such vote"
    | Some vote -> vote in
  if Current.time () < vote.start_date then
    failwith ("Vote has not started yet", vote.start_date);
  if Current.time () > vote.end_date then
    failwith ("Vote is over", vote.end_date);
  if not (Map.mem vote_choice vote.actions) then failwith "Invalid vote choice";
  (* TODO maybe don't allow votes for zero balance at vote time *)
  let vote = vote.cast_votes <-
      Map.add (Current.sender ()) vote_choice vote.cast_votes in
  let storage = storage.votes <- Map.add vote_name vote storage.votes in
  [], storage

type vote_result = Draw of nat | Winner of (string * nat)

let find_winner cast_votes accounts =
  let tally, quorum =
    Map.fold (fun ((address, choice), (counts, quorum)) ->
        let stake = (get_account (address, accounts)).balance in
        let total_stake = match Map.find choice counts with
          | None -> stake
          | Some prev_stake -> stake + prev_stake in
        Map.add choice total_stake counts, quorum + stake
      ) cast_votes (Map, 0p) in
  let winner =
    Map.fold (fun ((choice, total), winner) ->
        match winner with
        | Draw prev_total ->
          if prev_total >= total then winner
          else Winner (choice, total)
        | Winner (_, prev_total) ->
          if prev_total = total then Draw total
          else if prev_total > total then winner
          else Winner (choice, total)
      ) tally (Draw 0p) in
  winner, quorum

(** Tally votes and execute action corresponding to winning option. If
    draw then nothing is executed. In all cases, the vote is removed
    from the ongoing set. *)
let%entry tallyVote vote_name storage =
  let vote = match Map.find vote_name storage.votes with
    | None -> failwith "No such vote"
    | Some vote -> vote in
  if Current.time () <= vote.end_date then
    failwith ("Vote is not over yet", vote.end_date);
  (* close vote *)
  let storage = storage.votes <- Map.remove vote_name storage.votes in
  let winner, quorum = find_winner vote.cast_votes storage.accounts in
  if quorum < vote.min_quorum then [], storage
  else
    match winner with
    | Draw _ ->
      (* Draw: do nothing *)
      [], storage
    | Winner (choice, _) ->
      let action = match Map.find choice vote.actions with
        | None -> failwith ()
        | Some a -> a in
      action (), storage
