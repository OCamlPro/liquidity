
(* transfers *)

[%%version 0.15]

let%entry main
      (parameter : unit contract)
      (storage : tez) =

      let amount = Current.amount () in
      let storage = storage + amount in
      let op = Contract.call parameter amount () in

      ( [op], storage )
