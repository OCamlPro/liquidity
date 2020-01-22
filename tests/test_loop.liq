
(* loops *)


type storage = int

let%entry default
      (parameter : int)
      storage =

      let storage =
        Loop.loop (fun x ->
            ( false, x+1 )
        ) parameter
      in

      ( [], storage )

