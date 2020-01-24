
(* transfers *)


type storage = tez

let%entry default
      (parameter : [%handle: unit] )
      storage =

      let amount = Current.amount () in
      let storage = storage + amount in
      let op = Contract.call parameter amount () in

      ( [op], storage )
