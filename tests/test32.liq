[%%version 0.12]

type t = A of int | B of (int * ((bool * unit) * nat))

let%entry main
    (parameter : t)
    (storage : int)
  : int * int =

  match parameter with
  | A i ->
    (i, storage)
  | B (_, ((b, _), n)) ->
    if b then (int n, storage)
    else (0, storage)
