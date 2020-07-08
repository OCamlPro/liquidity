type storage = address

contract S = struct

  contract A = struct
    type storage = { a : nat; b : nat }

    let%entry default () s =
      if s.a = s.b then failwith ();
      [], s
  end

  type xx = {w : int; r : bool }
  type storage = { some_tez : tez; some_string : string ; x : xx}

  let%entry default ((delegate : key_hash), (manager : key_hash)) s =
    if s.some_tez < 10tz then failwith "less-than-ten" ;
    let delegatable = true in
    let spendable = false in
    let (initial : A.storage) = { A.a = 0p; A.b = 1p } in
    let (op, _addr) =
      Contract.create ~storage:initial
        ~delegate:(Some delegate) ~amount:10tz (contract A) in
    [op], s
end

let%entry default ((delegate : key_hash), (manager : key_hash)) _ =
  let delegatable = true in
  let spendable = false in
  let initial_storage = { S.some_tez = 10tz; S.some_string = "Hello" ; S.x = { S.w = 0; S.r = true }} in
  let (op, addr) =
    Contract.create ~storage:initial_storage
      ~delegate:(Some delegate) ~amount:10tz (contract S) in
  [op], addr
