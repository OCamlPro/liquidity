
(* loops *)

[%%version 0.4]

type storage = int

let%entry main
      (parameter : int)
      (storage : int) =

      let storage =
        Loop.loop (fun x ->
            ( false, x+1 )
        ) parameter
      in

      ( ([] : operation list), storage )

