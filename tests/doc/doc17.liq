type storage = UnitContract.instance option

let%entry main (k : key_hash) _ =
  let my_contract = Account.default k in
  let op = my_contract.main () ~amount:0DUN in
  [op], Some my_contract
