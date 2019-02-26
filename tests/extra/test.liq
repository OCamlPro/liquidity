[%%version 0.404]

contract M = struct
  let r = 9p
end

type storage = tez

let%entry main (c, k, a) (* ((c : UnitContract.instance), (k:key), (a:address)) *) s =
  let s = Extra.get_balance c in
  let op = Extra.reveal a k in
  Extra.increase_balance 1_000_000tz;
  (match Extra.get_storage c with
   | Some x -> if Extra.f x < M.r + Extra.w then () else Extra.test_fail k
   | None -> Extra.test_fail "okokokokok");
  if Extra.block_level () < 999p then failwith ();
  [op], s
