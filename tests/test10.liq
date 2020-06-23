
(* constructors *)


type storage =
        bool *
        int option *
        (string,int) map

let x = if true then 3 else 4

let%entry default
      (p : bool)
      (storage : storage)
      =
      begin [@fee
        if x > 3 && storage.(0) && p then (10tz, 10p) else (0tz, 0p)
      ]

        (* options *)
        let option = Some x in
        let storage  = Array.set storage 1 option in

        ( [], storage )

      end
