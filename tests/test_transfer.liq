
(* transfers *)

[%%version 0.403]

type storage = tez

let%entry main
      (dest : key_hash)
      (storage : tez) =

      let storage = storage + Current.amount () in
      let amount = storage in
      let op = Account.transfer dest amount in

      ( [op], storage )
