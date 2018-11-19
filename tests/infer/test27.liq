[%%version 0.4]

contract type NatContract = sig
  type storage
  val%entry thisisit : nat -> storage -> (operation list * storage)
end

type storage = bool * NatContract.instance

let%entry main (parameter : NatContract.instance) storage =

  let (b, c) = storage in
  if b then
    [], set storage 0 false
  else
    let op = c.thisisit ~amount:0tz 0p in
    [op], set storage 0 true
