type storage = {
  x : int;
  my_address : address;
}

let%entry main _ storage =
  let addr = Contract.address (Contract.self ()) in
  let storage = storage.my_address <- addr in
  ([] : operation list), storage
