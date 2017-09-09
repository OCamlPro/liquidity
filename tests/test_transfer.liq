
(* transfers *)

(* let version = "1.0" *)

let contract
      (amount : tez)
      (parameter : (unit,unit) contract)
      (storage : tez)
      (return : unit) =

      let storage = storage + amount in
      let (result, storage) =
          Contract.call parameter amount storage () in

      ( result, storage )
