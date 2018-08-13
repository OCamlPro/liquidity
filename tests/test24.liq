[%%version 0.3]

type t = A of int | B | C of (int * nat)

type storage = int

contract type UnitContract = sig
  type storage
contract type U = sig
  type storage
  val%entry main : parameter:unit -> storage -> (operation list * storage)
end
  val%entry main : parameter:unit -> storage -> (operation list * storage)
end


let%entry main
    (parameter : (contract UnitContract.U) * t)
    (storage : int)  =

  let (c, m) = parameter in
  match m with
  | A _ ->
    let amount = 0tz in
    let op = Contract.transfer ~dest:c ~amount in
    [op], storage
  | B ->
    let op = Contract.call c 0tz main ~parameter:() in
    [op], 0
  | C _ ->
    let op = Contract.call c 1tz () in
    [op], 1
