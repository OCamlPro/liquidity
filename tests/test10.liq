
(* constructors *)


type storage =
        bool *
        int option *
        (string,int) map

let%entry main
      (_parameter : bool)
      (storage : storage)
      =
      begin [@fee
        if _parameter then (10tz, 10p) else (0tz, 0p)
      ]

        (* options *)
        let x = 3 in
        let option = Some x in
        let storage  = Array.set storage 1 option in

        ( [], storage )

      end
