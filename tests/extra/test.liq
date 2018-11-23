[%%version 0.404]

type storage = tez

let%entry main ((c : UnitContract.instance), (k:key), (a:address)) s =
  let s = Extra.get_balance c in
  let op = Extra.reveal a k in
  Extra.increase_balance 1_000_000tz;
  if Extra.block_level () < 999p then failwith ();
  [op], s
