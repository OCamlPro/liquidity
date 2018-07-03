(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS <contact@ocamlpro.com>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

[%%version 0.15]

type pending_state = {
  yetNeeded : nat;
  ownersDone : key_hash set;
}

type transaction = {
  destination : key_hash;
  amount : tez;
}

type operation =
  | Transaction of transaction
  | ChangeOwner of key_hash * key_hash
  | AddOwner of key_hash
  | RemoveOwner of key_hash
  | ChangeRequirement of nat
  | SetDailyLimit of tez
  | ResetSpentToday
  | Kill of key_hash

type parameter =
  | Operation of operation
  | Deposit
  | Revoke of operation

type operation_hash = string

type storage = {
  m_required : nat;
  m_num_owners : nat;
  m_owners : key_hash set;
  m_pending : (operation_hash, pending_state) map;
  m_daily_limit : tez;
  m_spent_today : tez;
  m_last : timestamp;
}

let is_owner ((addr : key_hash), (storage:storage)) =
  Set.mem addr storage.m_owners

let onlyowner (storage:storage) =
  if is_owner (Contract.manager (Source : (unit, unit) contract), storage)
  then ()
  else Current.failwith ()

let clearPending (storage : storage) =
  let m_pending =
    Map.fold (fun ((op, _), m_pending) ->
        Map.remove op m_pending
      ) storage.m_pending storage.m_pending
  in
  storage.m_pending <- m_pending

type check_result =
  | Confirmations_Needed of storage
  | Execute of storage

let confirmAndCheck ((op : operation), (storage: storage)) =
  let owner = Contract.manager (Source : (unit, unit) contract) in
  if not (is_owner (owner, storage)) then Current.failwith ();
  let oph = Crypto.hash (Bytes.pack op) in
  let pending = match Map.find oph storage.m_pending with
    | None ->
      (* New operation *)
      { yetNeeded = storage.m_required;
        ownersDone = (Set : key_hash set);
      }
    | Some pending ->
      (* Operation already registered but not executed *)
      pending
  in
  (* make sure we (the message sender) haven't confirmed this operation
     previously *)
  if Set.mem owner pending.ownersDone then Current.failwith ();
  (* check if count is enough to go ahead *)
  if pending.yetNeeded <= 1p then
    let storage =
      storage.m_pending <- (Map : (operation_hash, pending_state) map) in
    Execute storage
  else
    let new_yetNeeded = match%nat int pending.yetNeeded - 1 with
      | Plus n -> n
      | Minus _ -> Current.failwith ()
    in
    let pending = pending.yetNeeded <- new_yetNeeded in
    let pending = pending.ownersDone <- Set.add owner pending.ownersDone in
    let storage = storage.m_pending <- Map.add oph pending storage.m_pending in
    Confirmations_Needed storage

let only_enough_owners ((op : operation), (storage: storage)) =
  match confirmAndCheck (op, storage) with
  | Confirmations_Needed _ -> Current.failwith ()
  | Execute storage -> storage

let has_confirmed ((op: operation), (owner: key_hash), (storage:storage)) =
  match Map.find (Crypto.hash (Bytes.pack op)) storage.m_pending with
  | None -> false
  | Some pending -> Set.mem owner pending.ownersDone



(* Revokes a prior confirmation of the given operation *)
let revoke
    ((op : operation),
     (storage : storage)) =
  onlyowner storage;
  let owner = Contract.manager (Source : (unit, unit) contract) in
  let oph = Crypto.hash (Bytes.pack op) in
  match Map.find oph storage.m_pending with
  | None -> Current.failwith ()
  | Some pending ->
    let storage =
      if Set.mem owner pending.ownersDone then
        let pending = pending.yetNeeded <- pending.yetNeeded + 1p in
        let pending =
          pending.ownersDone <- Set.remove owner pending.ownersDone in
        storage.m_pending <- Map.add oph pending storage.m_pending
      else storage
    in
    ((), storage)


let changeOwner
    ((parameter : key_hash * key_hash),
     (storage : storage)) =
  (* let storage = only_enough_owners (ChangeOwner parameter, storage) in *)
  match confirmAndCheck (ChangeOwner parameter, storage) with
  | Confirmations_Needed storage -> (), storage
  | Execute storage ->
    let from, _to = parameter in
    if is_owner (_to, storage) || not (is_owner (from, storage))
    then Current.failwith ()
    else
      let storage = clearPending storage in
      let storage = storage.m_owners <- Set.remove from storage.m_owners in
      let storage = storage.m_owners <- Set.add _to storage.m_owners in
      ((), storage)


let addOwner
    ((owner : key_hash),
     (storage : storage)) =
  (* let storage = only_enough_owners (AddOwner owner, storage) in *)
  match confirmAndCheck (AddOwner owner, storage) with
  | Confirmations_Needed storage -> (), storage
  | Execute storage ->
    if is_owner (owner, storage) then Current.failwith ();
    let storage = clearPending storage in
    let storage = storage.m_num_owners <- storage.m_num_owners + 1p in
    let storage = storage.m_owners <- Set.add owner storage.m_owners in
    ((), storage)


let removeOwner
    ((owner : key_hash),
     (storage : storage)) =
  (* let storage = only_enough_owners (RemoveOwner owner, storage) in *)
  match confirmAndCheck (RemoveOwner owner, storage) with
  | Confirmations_Needed storage -> (), storage
  | Execute storage ->
    if not (is_owner (owner,storage)) then Current.failwith ();
    if storage.m_required >= storage.m_num_owners then Current.failwith ();
    let new_num_owners = match%nat int storage.m_num_owners - 1 with
      | Plus n -> n
      | Minus _ -> Current.failwith ()
    in
    let storage = storage.m_num_owners <- new_num_owners in
    let storage = storage.m_owners <- Set.remove owner storage.m_owners in
    let storage = clearPending storage in
    ((), storage)


let changeRequirement
    ((required : nat),
     (storage : storage)) =
  (* let storage = only_enough_owners (ChangeRequirement required, storage) in *)
  match confirmAndCheck (ChangeRequirement required, storage) with
  | Confirmations_Needed storage -> (), storage
  | Execute storage ->
    if required > storage.m_num_owners then Current.failwith ();
    let storage = clearPending storage in
    let storage = storage.m_required <- required in
    ((), storage)

let underLimit ((value: tez), (storage: storage)) =
  onlyowner storage;
  let storage =
    if Current.time () >= storage.m_last + 86400 (* 24h *) then
      let storage = storage.m_spent_today <- 0tz in
      storage.m_last <- Current.time ()
    else storage
  in
  if storage.m_spent_today + value <= storage.m_daily_limit then
    true, storage.m_spent_today <- storage.m_spent_today + value
  else
    false, storage


let%init storage (owners : key_hash set) (required : nat) (limit : tez) =
  let m_owners = Set.add (Contract.manager (Source : (unit, unit) contract)) owners in
  let m_num_owners = Set.size m_owners in
  if required > m_num_owners then Current.failwith ();
  {
    m_num_owners;
    m_owners;
    m_required = required;
    m_pending = (Map : (operation_hash, pending_state) map);
    m_daily_limit = limit;
    m_spent_today = 0tz;
    m_last = Current.time ();
  }

(*
let%init storage =
{
  m_num_owners = 1p;
  m_owners = Set [ tz1XT2pgiSRWQqjHv5cefW7oacdaXmCVTKrU ];
  m_required = 1p;
  m_pending = (Map : (operation_hash, pending_state) map);
  m_daily_limit = 10tz;
  m_spent_today = 0tz;
  m_last = 2017-12-06T10:23:55+01:00;
}
*)



let setDailyLimit
    ((limit : tez),
     (storage : storage)) =
  (* let storage = storage.multi <- *)
  (*     MultiOwned.only_enough_owners *)
  (*       (MultiOwned.SetDailyLimit limit, storage.multi) in *)
  match confirmAndCheck (SetDailyLimit limit, storage) with
  | Confirmations_Needed storage -> (), storage
  | Execute storage -> (), storage.m_daily_limit <- limit


let resetSpentToday (storage : storage) =
  (* let storage = storage.multi <- *)
  (*     MultiOwned.only_enough_owners *)
  (*       (MultiOwned.ResetSpentToday, storage.multi) in *)
  match confirmAndCheck (ResetSpentToday, storage) with
  | Confirmations_Needed storage -> (), storage
  | Execute storage -> (), storage.m_spent_today <- 0tz

let deposit (storage : storage) =
  if Current.amount () = 0tz then Current.failwith ()
  else ((), storage)

(*
let%entry kill
    (parameter : key_hash)
    (storage : storage) : unit * storage =
  match confirmAndCheck (Kill parameter, storage) with
  | Confirmations_Needed storage -> (), storage
  | Execute storage ->
    let c_dest = Account.default parameter in
    let _, storage = Contract.call c_dest (Current.balance ()) storage () in
    (), storage


let%entry execute
    (parameter : key_hash, tez)
    (storage : storage) : unit * storage =
  onlyowner storage;
  let destination, amount = parameter in
  (* execute directly if under limit *)
  let under, storage = underLimit (amount, storage) in
  if under then
    let c_dest = Account.default destination in
    let _, storage = Contract.call c_dest amount storage () in
    (), storage
  else
    match confirmAndCheck (Transaction { destination; amount }, storage) with
    | Confirmations_Needed storage -> (), storage
    | Execute storage ->
      let c_dest = Account.default destination in
      let _, storage = Contract.call c_dest amount storage () in
      (), storage
*)

let%entry main
    (parameter : parameter)
    (storage : storage) : unit * storage =
  match parameter with
  | Deposit -> deposit storage
  | Revoke op -> revoke (op, storage)
  | Operation op -> match op with
    | ChangeOwner from_to -> changeOwner (from_to, storage)
    | AddOwner owner -> addOwner (owner, storage)
    | RemoveOwner owner -> removeOwner (owner, storage)
    | ChangeRequirement req -> changeRequirement (req, storage)
    | SetDailyLimit l -> setDailyLimit (l, storage)
    | ResetSpentToday -> resetSpentToday storage

    | Kill dest ->
      begin match confirmAndCheck (Kill dest, storage) with
        | Confirmations_Needed storage -> (), storage
        | Execute storage ->
          let c_dest = Account.default dest in
          let _, storage = Contract.call c_dest (Current.balance ()) storage () in
          (), storage
      end

    | Transaction t ->
      onlyowner storage;
      (* execute directly if under limit *)
      let under, storage = underLimit (t.amount, storage) in
      if under then
        let c_dest = Account.default t.destination in
        let _, storage = Contract.call c_dest t.amount storage () in
        (), storage
      else
        match confirmAndCheck (Transaction t, storage) with
        | Confirmations_Needed storage -> (), storage
        | Execute storage ->
          let c_dest = Account.default t.destination in
          let _, storage = Contract.call c_dest t.amount storage () in
          (), storage
