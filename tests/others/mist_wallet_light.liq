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
  destination : address;
  amount : tez;
}

type wallet_operation =
  | Withdraw of transaction
  | ChangeOwner of address * address
  | AddOwner of address
  | RemoveOwner of address
  | ChangeRequirement of nat

type operation_bytes = bytes

type storage = {
  m_required : nat;
  m_num_owners : nat;
  m_owners : address set;
  m_pending : (operation_bytes, pending_state) map;
}

let%init storage (owners : address set) (required : nat) =
  let m_num_owners = Set.size owners in
  if required > m_num_owners then Current.failwith ();
  {
    m_num_owners;
    m_owners = owners;
    m_required = required;
    m_pending = Map;
  }

let[@inline] no_op = []

let[@inline] is_owner (addr, storage) =
  Set.mem addr storage.m_owners

let[@inline] onlyowner storage =
  if is_owner (Current.source (), storage)
  then ()
  else Current.failwith ()

let[@inline] clearPending storage =
  storage.m_pending <- Map

type check_result =
  | Confirmations_Needed of storage
  | Execute of storage

let confirmAndCheck (op, storage) =
  let owner = Current.source () in
  if not (is_owner (owner, storage)) then Current.failwith ();
  let op_bytes = Bytes.pack op in
  let pending = match Map.find op_bytes storage.m_pending with
    | None ->
      (* New operation *)
      { yetNeeded = storage.m_required;
        ownersDone = Set;
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
      storage.m_pending <- Map in
    Execute storage
  else
    let new_yetNeeded = match%nat int pending.yetNeeded - 1 with
      | Plus n -> n
      | Minus _ -> Current.failwith ()
    in
    let pending = pending.yetNeeded <- new_yetNeeded in
    let pending = pending.ownersDone <- Set.add owner pending.ownersDone in
    let storage = storage.m_pending <- Map.add op_bytes pending storage.m_pending in
    Confirmations_Needed storage

(* Revokes a prior confirmation of the given operation *)
let%entry revoke (op : wallet_operation) storage =
  onlyowner storage;
  let owner = Current.source () in
  let op_bytes = Bytes.pack op in
  match Map.find op_bytes storage.m_pending with
  | None -> Current.failwith ()
  | Some pending ->
    if Set.mem owner pending.ownersDone then
      let pending = pending.yetNeeded <- pending.yetNeeded + 1p in
      let pending =
        pending.ownersDone <- Set.remove owner pending.ownersDone in
      no_op, storage.m_pending <- Map.add op_bytes pending storage.m_pending
    else no_op, storage



let%entry changeOwner (parameter : address * address) storage =
  (* let storage = only_enough_owners (ChangeOwner parameter, storage) in *)
  match confirmAndCheck (ChangeOwner parameter, storage) with
  | Confirmations_Needed storage -> no_op, storage
  | Execute storage ->
    let from, _to = parameter in
    if is_owner (_to, storage) || not (is_owner (from, storage))
    then Current.failwith ()
    else
      let storage = clearPending storage in
      let storage = storage.m_owners <- Set.remove from storage.m_owners in
      no_op, storage.m_owners <- Set.add _to storage.m_owners


let%entry addOwner (owner : address) storage =
  (* let storage = only_enough_owners (AddOwner owner, storage) in *)
  match confirmAndCheck (AddOwner owner, storage) with
  | Confirmations_Needed storage -> no_op, storage
  | Execute storage ->
    if is_owner (owner, storage) then Current.failwith ();
    let storage = clearPending storage in
    let storage = storage.m_num_owners <- storage.m_num_owners + 1p in
    no_op, storage.m_owners <- Set.add owner storage.m_owners


let%entry removeOwner (owner : address) storage =
  (* let storage = only_enough_owners (RemoveOwner owner, storage) in *)
  match confirmAndCheck (RemoveOwner owner, storage) with
  | Confirmations_Needed storage -> no_op, storage
  | Execute storage ->
    if not (is_owner (owner,storage)) then Current.failwith ();
    if storage.m_required >= storage.m_num_owners then Current.failwith ();
    let new_num_owners = match%nat int storage.m_num_owners - 1 with
      | Plus n -> n
      | Minus _ -> Current.failwith ()
    in
    let storage = storage.m_num_owners <- new_num_owners in
    let storage = storage.m_owners <- Set.remove owner storage.m_owners in
    no_op, clearPending storage


let%entry changeRequirement (required : nat) storage =
  (* let storage = only_enough_owners (ChangeRequirement required, storage) in *)
  match confirmAndCheck (ChangeRequirement required, storage) with
  | Confirmations_Needed storage -> no_op, storage
  | Execute storage ->
    if required > storage.m_num_owners then Current.failwith ();
    let storage = clearPending storage in
    no_op, storage.m_required <- required

let%entry deposit () storage =
  if Current.amount () = 0tz then Current.failwith ()
  else no_op, storage

let%entry withdraw (t: transaction) storage =
  onlyowner storage;
  match confirmAndCheck (Withdraw t, storage) with
  | Confirmations_Needed storage -> no_op, storage
  | Execute storage ->
    let op = t.destination.default ~amount:t.amount () in
    [op], storage
