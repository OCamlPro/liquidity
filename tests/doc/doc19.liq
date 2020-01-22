type storage = {
  x : int;
  my_address : address;
}

let%entry default () storage =
  let addr = Contract.self () in
  let storage = storage.my_address <- addr in
  [], storage
