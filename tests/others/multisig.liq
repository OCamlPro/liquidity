
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
  let owners_length = Set.size owners in
  if min_agree > owners_length then
    Current.failwith "Number of owners must be greater or equal to quorum";
  {
    owners;
    owners_length;
    min_agree;
    actions = Map;
  }

(* fails if the proposition is not a valid one *)
let[@inline] check_proposition p =
  if p.amount > Current.balance () then
    Current.failwith "Balance to low for withdrawal"
  else ()

(* returns true if two proositions are identical *)
let[@inline] equal_props (p1, p2) =
  p1.destination = p2.destination &&
  p1.amount = p2.amount

(* returns true if a proposition p should be executed immediately *)
let should_execute (p, storage) =
  let nb_agree =
    Map.fold (fun ((_, p'), cpt) ->
        if equal_props (p, p') then cpt + 1p else cpt
      ) storage.actions 0p
  in
  nb_agree >= storage.min_agree

let%entry pay (parameter : unit) storage =
  begin[@fee
    failwith "I don't pay fees for the entry point pay"
  ]
    (* Simple payment, nothing to do *)
    [], storage
  end

let%entry manage (action : action) storage =
  begin[@fee
    match action with
    | None -> 0.01tz, 0p
    | Some _ -> 0.15tz, 10p
  ]
    (* Owner wants to perform an action *)
    let owner = Current.source () in
    (* the caller must be an owner*)
    if not (Set.mem owner storage.owners) then
      Current.failwith ("Not an owner", owner);
    (* Don't send money while managing multisig *)
    if Current.amount () <> 0tz then
      Current.failwith "Don't send money while managing multisig";
    (* Register the desired action in the storage *)
    let storage =
      storage.actions <- Map.update owner action storage.actions in
    let do_nothing = [], storage in
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
        [op], storage.actions <- Map []
      else
        do_nothing
  end
