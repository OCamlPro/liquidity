type storage = unit

let%entry main () _ =
  let op =
    Account.transfer ~dest:dn1UqnHgHFe8ezEgsoow4hERctPssuWiw9h8 ~amount:1DUN in
  [op], ()
