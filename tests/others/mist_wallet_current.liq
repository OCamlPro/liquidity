(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS <contact@ocamlpro.com>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type pending_state = {
  yetNeeded : nat;
  ownersDone : address set;
}

type transaction = {
  destination : (contract UnitContract);
  amount : tez;
}

type wallet_operation =
  | Transaction of transaction
  | ChangeOwner of address * address
  | AddOwner of address
  | RemoveOwner of address
  | ChangeRequirement of nat
  | SetDailyLimit of tez
  | ResetSpentToday
  | Kill of (contract UnitContract)

type parameter =
  | Operation of wallet_operation
  | Deposit
  | Revoke of wallet_operation

type operation_hash = bytes

type storage = {
  m_required : nat;
  m_num_owners : nat;
  m_owners : address set;
  m_pending : (operation_hash, pending_state) map;
  m_daily_limit : tez;
  m_spent_today : tez;
  m_last : timestamp;
}

let no_op = ([] : operation list) [@@inline]

let is_owner ((addr : address), (storage:storage)) =
  Set.mem addr storage.m_owners
[@@inline]

let onlyowner (storage:storage) =
  if is_owner (Current.source (), storage)
  then ()
  else Current.failwith ()
[@@inline]

let clearPending (storage : storage) =
  storage.m_pending <- (Map : (operation_hash, pending_state) map)
[@@inline]

type check_result =
  | Confirmations_Needed of storage
  | Execute of storage

let confirmAndCheck ((op : wallet_operation), (storage: storage)) =
  let owner = Current.source () in
  if not (is_owner (owner, storage)) then Current.failwith ();
  let oph = Crypto.blake2b (Bytes.pack op) in
  let pending = match Map.find oph storage.m_pending with
    | None ->
      (* New operation *)
      { yetNeeded = storage.m_required;
        ownersDone = (Set : address set);
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
[@@inline]

(*
let only_enough_owners ((op : wallet_operation), (storage: storage)) =
  match confirmAndCheck (op, storage) with
  | Confirmations_Needed _ -> Current.failwith ()
  | Execute storage -> storage
[@@inline]
*)

(*
let has_confirmed ((op: wallet_operation), (owner: address), (storage:storage)) =
  match Map.find (Crypto.blake2b (Bytes.pack op)) storage.m_pending with
  | None -> false
  | Some pending -> Set.mem owner pending.ownersDone
[@@inline]
*)


(* Revokes a prior confirmation of the given operation *)
let revoke
    ((op : wallet_operation),
     (storage : storage)) =
  onlyowner storage;
  let owner = Current.source () in
  let oph = Crypto.blake2b (Bytes.pack op) in
  match Map.find oph storage.m_pending with
  | None -> Current.failwith ()
  | Some pending ->
    if Set.mem owner pending.ownersDone then
      let pending = pending.yetNeeded <- pending.yetNeeded + 1p in
      let pending =
        pending.ownersDone <- Set.remove owner pending.ownersDone in
      storage.m_pending <- Map.add oph pending storage.m_pending
    else storage



let changeOwner
    ((parameter : address * address),
     (storage : storage)) =
  (* let storage = only_enough_owners (ChangeOwner parameter, storage) in *)
  match confirmAndCheck (ChangeOwner parameter, storage) with
  | Confirmations_Needed storage -> storage
  | Execute storage ->
    let from, _to = parameter in
    if is_owner (_to, storage) || not (is_owner (from, storage))
    then Current.failwith ()
    else
      let storage = clearPending storage in
      let storage = storage.m_owners <- Set.remove from storage.m_owners in
      storage.m_owners <- Set.add _to storage.m_owners


let addOwner
    ((owner : address),
     (storage : storage)) =
  (* let storage = only_enough_owners (AddOwner owner, storage) in *)
  match confirmAndCheck (AddOwner owner, storage) with
  | Confirmations_Needed storage -> storage
  | Execute storage ->
    if is_owner (owner, storage) then Current.failwith ();
    let storage = clearPending storage in
    let storage = storage.m_num_owners <- storage.m_num_owners + 1p in
    storage.m_owners <- Set.add owner storage.m_owners
[@@inline]


let removeOwner
    ((owner : address),
     (storage : storage)) =
  (* let storage = only_enough_owners (RemoveOwner owner, storage) in *)
  match confirmAndCheck (RemoveOwner owner, storage) with
  | Confirmations_Needed storage -> storage
  | Execute storage ->
    if not (is_owner (owner,storage)) then Current.failwith ();
    if storage.m_required >= storage.m_num_owners then Current.failwith ();
    let new_num_owners = match%nat int storage.m_num_owners - 1 with
      | Plus n -> n
      | Minus _ -> Current.failwith ()
    in
    let storage = storage.m_num_owners <- new_num_owners in
    let storage = storage.m_owners <- Set.remove owner storage.m_owners in
    clearPending storage
[@@inline]


let changeRequirement
    ((required : nat),
     (storage : storage)) =
  (* let storage = only_enough_owners (ChangeRequirement required, storage) in *)
  match confirmAndCheck (ChangeRequirement required, storage) with
  | Confirmations_Needed storage -> storage
  | Execute storage ->
    if required > storage.m_num_owners then Current.failwith ();
    let storage = clearPending storage in
    storage.m_required <- required

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
[@@inline]

let%init storage (owners : address set) (required : nat) (limit : tez) =
  let m_owners = Set.add (Current.source ()) owners in
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
  | Confirmations_Needed storage -> storage
  | Execute storage -> storage.m_daily_limit <- limit


let resetSpentToday (storage : storage) =
  (* let storage = storage.multi <- *)
  (*     MultiOwned.only_enough_owners *)
  (*       (MultiOwned.ResetSpentToday, storage.multi) in *)
  match confirmAndCheck (ResetSpentToday, storage) with
  | Confirmations_Needed storage -> storage
  | Execute storage -> storage.m_spent_today <- 0tz

let deposit (storage : storage) =
  if Current.amount () = 0tz then Current.failwith ()
  else storage

let kill
    ((dest: (contract UnitContract)),
     (storage: storage)) =
  match confirmAndCheck (Kill dest, storage) with
  | Confirmations_Needed storage -> no_op, storage
  | Execute storage ->
    let trans_all = Contract.call dest (Current.balance ()) () in
    [trans_all], storage

let transaction
    ((t: transaction),
     (storage: storage)) =
  onlyowner storage;
  (* execute directly if under limit *)
  let under, storage = underLimit (t.amount, storage) in
  if under then
    let op = Contract.call t.destination t.amount () in
    [op], storage
  else
    match confirmAndCheck (Transaction t, storage) with
    | Confirmations_Needed storage -> no_op, storage
    | Execute storage ->
      let op = Contract.call t.destination t.amount () in
      [op], storage


let%entry main
    (parameter : parameter)
    (storage : storage) =
  match parameter with
  | Deposit -> no_op, deposit storage
  | Revoke op -> no_op, revoke (op, storage)
  | Operation op -> match op with
    | ChangeOwner from_to -> no_op, changeOwner (from_to, storage)
    | AddOwner owner -> no_op, addOwner (owner, storage)
    | RemoveOwner owner -> no_op, removeOwner (owner, storage)
    | ChangeRequirement req -> no_op, changeRequirement (req, storage)
    | SetDailyLimit l -> no_op, setDailyLimit (l, storage)
    | ResetSpentToday -> no_op, resetSpentToday storage
    | Kill dest -> kill (dest, storage)
    | Transaction t -> transaction (t, storage)
