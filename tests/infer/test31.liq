[%%version 0.4]

type storage = int

contract type I = sig
  type storage
  val%entry main : int -> storage -> (operation list * storage)
end


let%entry main (parameter : I.instance * 'a) storage =

  let (c, l) = parameter in
  match l with
  | None ->
    ([], storage)
  | Some x  ->
    let op = Contract.call c 0tz x in
    ([op], storage)
