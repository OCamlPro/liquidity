
(* constants *)

[%%version 0.2]

type storage =
        bool *
        int option *
        (string,int) map *
        int set *
        int list

let%entry main
      (parameter : bool)
      (storage : storage) =

(* booleans *)
      let bool =
        if parameter then true else false
      in
      let storage  = set storage 0 bool in

      ( ([] : operation list), storage )
