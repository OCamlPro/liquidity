type storage = address 
contract C = struct 
  type storage = A | B
  let%entry default (_delegate : key_hash) _ = [], A
end
let%entry default (delegate : key_hash) (s : storage) =
  let initial_storage = C.A in
  let (op, addr) =
    Contract.create ~storage:initial_storage ~delegate:(Some delegate) ~amount:10tz
      (contract C)
  in
  [], addr
