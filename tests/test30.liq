
type storage = int

let%entry default
    (parameter : address * int list)
    storage =

  let (c, l) = parameter in
  match l with
  | [] ->
    let op = c.default (-1) 0DUN in
    ([op], storage)
  | _ :: _ ->
    ([], storage)
