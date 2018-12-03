type storage = address option

let%entry main (k : key_hash) _ =
  let not_delegatable = false in
  let (op, addr) =
    Account.create
      ~manager:k ~delegate:(Some k) ~delegatable:false ~amount:100tz in
  ([op], Some addr)
