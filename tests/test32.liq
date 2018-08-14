[%%version 0.3]

type t = A of int | B of (int * ((bool * unit) * nat))

type storage = int

let%entry main
    (parameter : t)
    (storage : int) =

  let s = match parameter with
  | A i ->
    i
  | B (_, ((b, _), n)) ->
    if b then int n
    else storage
  in
  ([]:operation list), s
