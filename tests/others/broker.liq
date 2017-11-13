
[%%version 0.13]

type storage = {
  state : string;
  timeout : timestamp;
  pn : tez * tez;
  x : (unit, unit) contract;
  a : (unit, unit) contract;
  b : (unit, unit) contract;
}

let%entry main
      (parameter : timestamp)
      (storage : storage)
      : unit * storage =

  if storage.state <> "open" then Current.fail ()
  else
    if Current.time () < storage.timeout then (* Before timeout *)
      (* We compute ((1 + P) + N) tez for keeping the contract alive *)
      let (pn0, pn1) = storage.pn in
      let cost = 1.00tz + pn0 + pn1 in
      let b = Current.balance () in
      if cost < b then
        (* # Not enough cash, we just accept the transaction
           # and leave the global untouched *)
        ( (), storage )
      else
        (* # Enough cash, successful ending
                   # We update the global*)
        let storage = storage.state <- "success" in
        let (pn0, _) = storage.pn in
        let (_result, storage) =
          Contract.call storage.x pn0 storage () in
        let (_, pn1) = storage.pn in
        let (_result, storage) =
            Contract.call storage.a pn1 storage () in
        ( (), storage )
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
      let _result, storage =
          Contract.call storage.x transfer storage () in
      (* # We transfer the rest to B *)
      let transfer = Current.balance () - 1.00tz in
      let _result, storage =
          Contract.call storage.b transfer storage () in

      ( (), storage )
