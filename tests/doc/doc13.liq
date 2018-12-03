type storage = unit

let%entry main ( to_forward : tez ) _ =
  let dest = (tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc : UnitContract.instance) in
  let op = Contract.call ~dest ~amount:to_forward () in
  [op], ()
