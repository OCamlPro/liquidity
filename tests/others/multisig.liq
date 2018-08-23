[%%version 0.35]

(* A proposition of transfer to a destination, the address of (unit, unit)
   contract *)
type proposition = {
  destination : key_hash;
  amount : tez;
}

(* An owner can submit a proposition or remove a previously submitted
   proposition *)
type action = proposition option

type storage = {
  owners : address set;                 (* set of owners *)
  actions : (address, proposition) map; (* map of owner to proposition *)
  owners_length : nat;                  (* total number of owners *)
  min_agree : nat;                      (* minimum number of required
                                           agreements before proposition is
                                           executed *)
}

let%init storage (owners : address set) (min_agree : nat) =
  let owners = Set.add (Current.source ()) owners in
  let owners_length = Set.size owners in
  if min_agree > owners_length then
    Current.failwith "Number of owners must be greater or equal to quorum";
  {
    owners;
    owners_length;
    min_agree;
    actions = (Map : (address, proposition) map);
  }

(* fails if the proposition is not a valid one *)
let check_proposition (p:proposition) =
  if p.amount > Current.balance () then
    Current.failwith "Balance to low for withdrawal"
  else ()
[@@inline]

(* returns true if two proositions are identical *)
let equal_props ((p1:proposition), (p2:proposition)) =
  p1.destination = p2.destination &&
  p1.amount = p2.amount
[@@inline]

(* returns true if a proposition p should be executed immediately *)
let should_execute ((p : proposition), (storage : storage)) =
  let nb_agree =
    Map.fold (fun ((_, p'), cpt) ->
        if equal_props (p, p') then cpt + 1p else cpt
      ) storage.actions 0p
  in
  nb_agree >= storage.min_agree

let%entry pay (parameter : unit) storage =
  (* Simple payment, nothing to do *)
  ([] : operation list), storage

let%entry manage (action : action) storage =
  (* Owner wants to perform an action *)
  let owner = Current.source () in
  (* the caller must be an owner*)
  if not (Set.mem owner storage.owners) then Current.failwith ();
  (* Don't send money while managing multisig *)
  if Current.amount () <> 0tz then
    Current.failwith "Don't send money while managing multisig";
  (* Register the desired action in the storage *)
  let storage =
    storage.actions <- Map.update owner action storage.actions in
  let do_nothing = ([] : operation list), storage in
  match action with
  | None ->
    (* action is to remove previous proposition: nothing to do *)
    do_nothing
  | Some p ->
    (* The action is new proposition *)
    check_proposition p; (* it must be a valid one *)
    if should_execute (p, storage) then
      (* execute proposition, i.e. transfer tokens *)
      let c_dest = Account.default p.destination in
      let op = Contract.call c_dest p.amount () in
      (* reset the map of actions *)
      [op], storage.actions <- (Map [] : (address, proposition) map)
    else
      do_nothing
