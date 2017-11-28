[%%version 0.13]

(* A proposition of transfer to a destination, the address of (unit, unit)
   contract *)
type proposition = {
  destination : key_hash;
  amount : tez;
}

(* An owner can submit a proposition or remove a previously submitted
   proposition *)
type action = proposition option

(* The multisig contract can be payed to, simply transfering tokens to it,
   or an owner can submit an action *)
type parameter =
  | Pay
  | Manage of action

type storage = {
  owners : key_hash set;                 (* set of owners *)
  actions : (key_hash, proposition) map; (* map of owner to proposition *)
  owners_length : nat;                   (* total number of owners *)
  min_agree : nat;                       (* minimum number of required
                                            agreements before proposition is
                                            executed *)
}

(* fails if the proposition is not a valid one *)
let check_proposition (p:proposition) =
  if p.amount > Current.balance () then Current.fail () else ()


(* returns the address of the current caller of this multisig contract *)
let get_caller (_ : unit) =
  let c = (Source : (unit, unit) contract) in
  Contract.manager c

(* returns true if two proositions are identical *)
let equal_props ((p1:proposition), (p2:proposition)) =
  p1.destination = p2.destination &&
  p1.amount = p2.amount

(* returns true if a proposition p should be executed immediately *)
let should_execute ((p : proposition), (storage : storage)) =
  let nb_agree =
    Map.fold (fun (((_:key_hash), (p':proposition)), (cpt:nat)) ->
        if equal_props (p, p') then cpt + 1p else cpt
      ) storage.actions 0p
  in
  nb_agree >= storage.min_agree


let%entry main
    (parameter : parameter)
    (storage : storage)
  : unit * storage
  =
  match parameter with
  | Pay ->
    (* Simple payment, nothing to do *)
    (), storage
  | Manage action ->
    (* Owner wants to perform an action *)
    let owner_kh = get_caller () in
    (* the caller must be an owner*)
    if not (Set.mem owner_kh storage.owners) then Current.fail ();
    (* Don't send money while managing multisig *)
    if Current.amount () <> 0tz then Current.fail ();
    (* Register the desired action in the storage *)
    let storage =
      storage.actions <- Map.update owner_kh action storage.actions in
    let storage =
      match action with
      | None ->
        (* action is to remove previous proposition: nothing to do *)
        storage
      | Some p ->
        (* The action is new proposition *)
        check_proposition p; (* it must be a valid one *)
        if should_execute (p, storage) then
          (* execute proposition, i.e. transfer tokens *)
          let c_dest = Account.default p.destination in
          let _, storage = Contract.call c_dest p.amount storage () in
          (* reset the map of actions *)
          storage.actions <- (Map [] : (key_hash, proposition) map)
        else
          storage
    in
    (), storage
