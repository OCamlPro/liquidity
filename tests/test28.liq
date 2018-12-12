[%%version 0.4]

contract type NatContract = sig
  type storage
  val%entry main : nat -> storage -> (operation list * storage)
end

type storage = bool * NatContract.instance

let%entry main (_parameter : NatContract.instance) storage =

  let (b, c) = storage in
  if b then
    let op = Contract.call c 0tz 0p in
    [op], set storage 0 true
  else
    [], set storage 0 false
