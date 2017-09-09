
(* loops *)

(* let version = "1.0" *)

let contract
      (amount : tez)
      (parameter : int)
      (storage : int)
      (return : unit) =

      let storage =
        Loop.loop (fun x ->
            ( false, x+1 )
        ) parameter
      in

      ( (), storage )

