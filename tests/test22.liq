[%%version 0.13]

type t = A of int | B | C of (int * nat)

let%entry main
    (parameter : (unit, unit) contract * t)
    (storage : int)
  : int * int =

  let (c, m) = parameter in
  match m with
  | A i ->
    (i, storage)
  | B ->
    let (_, storage) = Contract.call c 0tz storage () in
    (0, storage)
  | C _ ->
    let (_, storage) = Contract.call c 1tz storage () in
    (1, storage)
