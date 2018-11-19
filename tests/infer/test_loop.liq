
(* loops *)

[%%version 0.4]

type storage = int

let%entry main parameter storage =

      let storage =
        Loop.loop (fun x ->
            ( false, (x:int)+1 )
        ) parameter
      in

      [], storage

