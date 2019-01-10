
(* transfers *)

[%%version 0.5]

type storage = tez

let%entry main
      (dest : key_hash)
      storage =

      let storage = storage + Current.amount () in
      let amount = storage in
      let op = Account.transfer dest amount in

      ( [op], storage )
