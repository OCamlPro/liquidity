[%%version 0.3]

type storage = int

contract type I = sig
  type storage
  val%entry main : int -> storage -> (operation list * storage)
end


let%entry main
    (parameter : (contract I) * int list)
    (storage : int) =

  let (c, l) = parameter in
  match l with
  | [] ->
    (([]:operation list), storage)
  | x :: _ ->
    let op = Contract.call c 0tz x in
    [op], storage
