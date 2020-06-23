type storage = address option

let%entry default (k : key_hash) _ =
  let my_contract = Account.default k in
  let op = my_contract.default () ~amount:0DUN in
  [op], Some (Contract.address my_contract)
