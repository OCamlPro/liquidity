
type t = A of int | B | C of (int * nat)
type storage = int

let%entry default
    (parameter : address * t)
    storage =

  let (c, m) = parameter in
  match m with
  | A i ->
    [], i
  | B ->
    let _op = c.default () 0DUN in
    [], 0
  | C (i, _) ->
    let _op = c.default () 1DUN in
    [], i
