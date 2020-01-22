
type storage = int

contract type I = sig
  val%entry default : int
end


let%entry default
    (parameter : address * int option)
    storage =

  let (c, l) = parameter in
  match l with
  | None ->
    ([], storage)
  | Some x  ->
    let op = Contract.call c ~entry:default 0tz x in
    ([op], storage)
