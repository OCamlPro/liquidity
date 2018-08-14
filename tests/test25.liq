[%%version 0.3]

type storage = int

contract type N = sig
  type storage
  val%entry main : nat -> storage -> (operation list * storage)
end

let%entry main
    (parameter : (contract N))
    (storage : int) =

  let r, op = match%nat (storage + 1)  with
    | Plus x ->
      let op = Contract.call parameter 0tz x in
      (x + 2p, op)
    | Minus y ->
      let op = Contract.call parameter 0tz y in
      (y, op)
  in
  let x = r + 10p in
  ([op], int x)
