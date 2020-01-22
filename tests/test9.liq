
(* constants *)


type storage = bool *
               int option *
               (string,int) map *
               int set *
               int list

let%entry default
      (parameter : bool)
      storage =

(* booleans *)
      let bool =
        if parameter then true else false
      in
      let storage  = set storage 0 bool in
      
(* options *)
      let option =
        if parameter then
           None
        else
           Some 3
      in
      let storage  = set storage 1 option in

(* maps *)
      let map =
        if parameter then
           Map
        else 
        if parameter then
           Map [ "string", 1 ]
        else
           Map [ "string", 1; "toto",2 ]
      in
      let storage  = set storage 2 map in

(* sets *)
      let set =
        if parameter then
          Set
        else
          Set [1;2;3;4]
      in
      let storage = Array.set storage 3 set in

(* lists *)
      let list =
        if parameter or parameter then
          []
        else
          [1;2;3;4;5]
      in
      let storage = Array.set storage 4 list in

      ( [], storage )
