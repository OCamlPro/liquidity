
(* loops *)

[%%version 0.3]

let%entry main
      (parameter : int)
      (storage : int) =

      let storage =
        Loop.loop (fun x ->
            ( false, x+1 )
        ) parameter
      in

      ( ([] : operation list), storage )

