
(* loops *)

[%%version 0.1]

let%entry main
      (parameter : int)
      (storage : int)
      [%return : unit] =

      let storage =
        Loop.loop (fun x ->
            ( false, x+1 )
        ) parameter
      in

      ( (), storage )

