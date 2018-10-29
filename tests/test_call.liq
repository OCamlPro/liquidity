
(* transfers *)

[%%version 0.4]

type storage = tez

let%entry main
      (parameter : UnitContract.instance)
      (storage : tez) =

      let amount = Current.amount () in
      let storage = storage + amount in
      let op = Contract.call parameter amount () in

      ( [op], storage )
