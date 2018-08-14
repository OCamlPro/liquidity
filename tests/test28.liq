[%%version 0.3]

contract type NatContract = sig
  type storage
  val%entry main : nat -> storage -> (operation list * storage)
end

type storage = bool * (contract NatContract)

let%entry main (parameter : (contract NatContract)) storage =

  let (b, c) = storage in
  if b then
    let op = Contract.call c 0tz 0p in
    [op], set storage 0 true
  else
    ([]:operation list), set storage 0 false
