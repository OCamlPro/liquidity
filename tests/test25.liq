
type storage = int

let%entry default
    (parameter : address)
    storage =

  let handle = match [%handle: val%entry default : nat] parameter with
    | None -> failwith ()
    | Some h -> h in
  let r, op = match%nat (storage + 1)  with
    | Plus x ->
      let op = Contract.call handle 0tz x in
      (x + 2p, op)
    | Minus y ->
      let op = Contract.call handle 0tz y in
      (y, op)
  in
  let x = r + 10p in
  ([op], int x)
