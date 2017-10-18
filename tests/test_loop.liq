
(* loops *)

[%%version 0.12]

let%entry main
      (parameter : int)
      (storage : int)
      : unit * int =

      let storage =
        Loop.loop (fun x ->
            ( false, x+1 )
        ) parameter
      in

      ( (), storage )

