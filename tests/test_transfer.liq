
(* transfers *)

[%%version 0.13]

let%entry main
      (parameter : (unit,unit) contract)
      (storage : tez)
      : unit * tez =

      let amount = Current.amount () in
      let storage = storage + amount in
      let (result, storage) =
          Contract.call parameter amount storage () in

      ( result, storage )
