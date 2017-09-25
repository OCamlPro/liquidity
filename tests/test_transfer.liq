
(* transfers *)

let version = 1.0

let%entry main
      (parameter : (unit,unit) contract)
      (storage : tez)
      (return : unit) =

      let amount = Current.amount () in
      let storage = storage + amount in
      let (result, storage) =
          Contract.call parameter amount storage () in

      ( result, storage )
