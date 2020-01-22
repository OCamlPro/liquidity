
type storage = bool

let%entry default
    (parameter : address)
    _storage =

  let b =
   if
    let _op = Contract.call ~entry:default parameter 0tz 1p in
    true
   then
    false
   else
    true
  in
  [], b
