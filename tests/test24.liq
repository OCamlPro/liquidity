
type t = A of int | B | C of (int * nat)

type storage = int

contract type UnitContract = sig
  contract type U = sig
    val%entry default : unit
  end
  val%entry default : unit
end


let%entry default
    (parameter : address * t)
    storage =

  let (c, m) = parameter in
  let c = match [%handle UnitContract.U.default] c with
    | None -> failwith ()
    | Some c -> c in
  match m with
  | A _ ->
    let amount = 0tz in
    let op = Contract.call ~dest:c ~entry:default ~amount () in
    [op], storage
  | B ->
    let op = Contract.call c 0tz default ~parameter:() in
    [op], 0
  | C _ ->
    let op = c.default () ~amount:1tz in
    [op], 1
