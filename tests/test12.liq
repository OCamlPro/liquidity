
(* sets *)


type storage = string set

let%entry default
      (parameter : string)
      (storage : string set) =

      let set = Set in
      let set = Set.add "a" set in
      let set = Set.add "b" set in
      let set = Set.remove "c" set in
      let storage = if Set.mem parameter set then
           storage
        else
           set
       in

      ( [], storage )
