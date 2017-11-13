
(* sets *)

[%%version 0.13]

let%entry main
      (parameter : string)
      (storage : string set)
      : unit * string set =

      let set = (Set : string set) in
      let set = Set.update "a" true set in
      let set = Set.update "b" true set in
      let storage = if Set.mem parameter set then
           storage
        else
           set
       in

      ( (), storage )
