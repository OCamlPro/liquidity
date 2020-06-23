
type storage = bool * address

let%entry default (_parameter : address) storage =

  let (b, c) = storage in
  if b then
    let op = c.default 0p 0DUN in
    [op], set storage 0 true
  else
    [], set storage 0 false
