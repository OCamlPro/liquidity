
(* constants *)

[%%version 0.4]

type storage =
        bool *
        int option *
        (string,int) map *
        int set *
        int list

let%entry main
      (parameter : bool)
      storage =

(* booleans *)
      let bool =
        if parameter then true else false
      in
      let storage  = set storage 0 bool in

      ( [], storage )
