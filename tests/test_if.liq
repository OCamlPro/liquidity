
(* constants *)


type storage =
        bool *
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

      ( [], storage )
