
[%%version 0.2]

type storage = {
  state : string;
  timeout : timestamp;
  pn : tez * tez;
  x : unit contract;
  a : unit contract;
  b : unit contract;
}

let%entry main
      (parameter : timestamp)
      (storage : storage) =

  if storage.state <> "open" then Current.failwith ()
  else
    if Current.time () < storage.timeout then (* Before timeout *)
      (* We compute ((1 + P) + N) tez for keeping the contract alive *)
      let (pn0, pn1) = storage.pn in
      let cost = 1.00tz + pn0 + pn1 in
      let b = Current.balance () in
      if cost < b then
        (* # Not enough cash, we just accept the transaction
           # and leave the global untouched *)
        ( ([]: operation list), storage )
      else
        (* # Enough cash, successful ending
                   # We update the global*)
        let storage = storage.state <- "success" in
        let (pn0, _) = storage.pn in
        let op1 = Contract.call storage.x pn0 () in
        let (_, pn1) = storage.pn in
        let op2 = Contract.call storage.a pn1 () in
        ( [op1; op2], storage )
    else
      (* # After timeout, we refund
           # We update the global *)
      let (p, _) = storage.pn in
      let storage = storage.state <- "timeout" in
      (*  # We try to transfer the fee to the broker *)
      let bal = Current.balance () in
      let available = bal - 1.00tz in
      let transfer =
        if available < p then available
        else p
      in
      let op1 = Contract.call storage.x transfer () in
      (* # We transfer the rest to B *)
      let transfer = Current.balance () - 1.00tz in
      let op2 = Contract.call storage.b transfer () in

      ( [op1; op2], storage )
