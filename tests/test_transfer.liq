
(* transfers *)


type storage = tez

let%entry default
      (dest : key_hash)
      storage =

      let storage = storage + Current.amount () in
      let amount = storage in
      let op = Account.transfer dest amount in

      ( [op], storage )
