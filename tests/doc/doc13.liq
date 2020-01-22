type storage = unit

let%entry main ( to_forward : dun ) _ =
  let op =
    Contract.call
      ~dest:dn1UqnHgHFe8ezEgsoow4hERctPssuWiw9h8
      ~amount:to_forward () in
  [op], ()
