type storage = {
  x : int;
  c : address;
}

let%entry default () storage =
  let addr = Contract.untype (Contract.self ()) in
  let storage = storage.c <- addr in
  [], storage
