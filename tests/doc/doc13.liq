type storage = unit

let%entry main ( to_forward : dun ) _ =
  let dest = (dn1UqnHgHFe8ezEgsoow4hERctPssuWiw9h8 : UnitContract.instance) in
  let op = Contract.call ~dest ~amount:to_forward () in
  [op], ()
