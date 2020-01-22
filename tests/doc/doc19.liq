type storage = {
  x : int;
  my_address : address;
}

let%entry main () storage =
  let addr = Contract.self () in
  let storage = storage.my_address <- addr in
  [], storage
