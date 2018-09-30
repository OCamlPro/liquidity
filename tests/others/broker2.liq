
let version = 0.1

type storage = {
  state : string;
  timeout : timestamp;
  pn : tez * tez;
  x : (unit, unit) contract;
  a : (unit, unit) contract;
  b : (unit, unit) contract;
}

let contract
      (parameter : timestamp)
      (storage : storage)
      [%return : unit] =

  if storage.state <> "open" then Current.failwith ()
  else
    if Current.time () < storage.timeout then (* Before timeout *)
      (* We compute ((1 + P) + N) tez for keeping the contract alive *)
      let pn = storage.pn in
      let cost = ( "1.00" : tez ) + pn.(0) + pn.(1) in
      let b = Current.balance () in
      if cost < b then
        (* # Not enough cash, we just accept the transaction
           # and leave the global untouched *)
        ( (), storage )
      else
        (* # Enough cash, successful ending
                   # We update the global*)
        let storage = storage.state <- "success" in
        let (_result, storage) = Contract.call storage.x storage.pn.(0) storage () in
        let (_result, storage) = Contract.call storage.a storage.pn.(1) storage () in
        ( (), storage )
    else
      (* # After timeout, we refund
           # We update the global *)
      let p = storage.pn.(0) in
      let storage = storage.state <- "timeout" in
      (*  # We try to transfer the fee to the broker *)
      let bal = Current.balance () in
      let available = bal - ("1.00" : tez) in
      let transfer =
        if available < p then available
        else p
      in
      let _result, storage = Contract.call storage.x transfer storage () in
      (* # We transfer the rest to B *)
      let transfer = Current.balance () - ("1.00" : tez) in
      let _result, storage = Contract.call storage.b transfer storage () in

      ( (), storage )
