[%%version 0.404]

type storage = tez

let%entry main (c : UnitContract.instance) s =
  let s = Extra.get_balance c in
  ([] : operation list), s
