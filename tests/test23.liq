[%%version 0.3]

type storage = int

contract type N = sig
  type storage
  val%entry main : nat -> storage -> (operation list * storage)
end

let%entry main
    (parameter : (contract N))
    (storage : int) =

  let r, ops = match%nat (storage + 1)  with
    | Plus x -> x + 2p, ([]:operation list)
    | Minus y ->
      let op = Contract.call parameter 0tz y in
      0p, [op]
  in
  let x = r + 10p in
  let storage = abs storage in
  (if x > 10p then ops else ops), storage
