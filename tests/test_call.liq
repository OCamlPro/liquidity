
(* transfers *)

[%%version 0.5]

type storage = tez

let%entry main
      (parameter : UnitContract.instance)
      storage =

      let amount = Current.amount () in
      let storage = storage + amount in
      let op = Contract.call parameter amount () in

      ( [op], storage )
