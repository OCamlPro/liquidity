
(* constructors *)


type storage =
        bool *
        int option *
        (string,int) map

(* let fee = (10tz, 10p) *)

let%entry main
      (p : bool)
      (storage : storage)
      =
      begin [@fee
        if p then (10tz, 10p) else (0tz, 0p)
      ]

        (* options *)
        let x = if true then 3 else 4 in
        let option = Some x in
        let storage  = Array.set storage 1 option in

        ( [], storage )

      end
