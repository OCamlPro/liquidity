[%%version 0.4]

type storage = bool

contract type N = sig
  type storage
  val%entry main : nat -> storage -> (operation list * storage)
end

let%entry main
    (parameter : N.instance)
    _storage =

  let b =
   if
    let _op = Contract.call parameter 0tz 1p in
    true
   then
    false
   else
    true
  in
  [], b
