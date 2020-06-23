type storage = bool * address

let%entry default () storage =

  let (b, c) = storage in
  if b then
    [], set storage 0 false
  else
    let op = c.thisisit ~amount:0tz 0p in
    [op], set storage 0 true
