[%%version 0.3]

type storage = int

contract type I = sig
  type storage
  val%entry main : int -> storage -> (operation list * storage)
end


let%entry main
    (parameter : (contract I) * int option)
    (storage : int) =

  let (c, l) = parameter in
  match l with
  | None ->
    (([] : operation list), storage)
  | Some x  ->
    let op = Contract.call c 0tz x in
    ([op], storage)
