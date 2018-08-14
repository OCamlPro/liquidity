
(* transfers *)

[%%version 0.3]

type storage = tez

let%entry main
      (parameter : (contract UnitContract))
      (storage : tez) =

      let amount = Current.amount () in
      let storage = storage + amount in
      let op = Contract.call parameter amount () in

      ( [op], storage )
