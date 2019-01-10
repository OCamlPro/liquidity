[%%version 0.5]

type t = A of int | B of (int * ((bool * unit) * nat))

type storage = int

let%entry main
    (parameter : t)
    storage =

  let s = match parameter with
  | A i ->
    i
  | B (_, ((b, _), n)) ->
    if b then int n
    else storage
  in
  [], s
