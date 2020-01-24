type storage = unit

let%entry default ( to_forward : dun ) _ =
  let op =
    Contract.call
      ~dest:(dn1UqnHgHFe8ezEgsoow4hERctPssuWiw9h8 : address)
      ~entry:default
      ~amount:to_forward () in
  [op], ()
