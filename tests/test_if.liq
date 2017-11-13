
(* constants *)

[%%version 0.13]

type storage =
        bool *
        int option *
        (string,int) map *
        int set *
        int list

let%entry main
      (parameter : bool)
      (storage : storage)
      : unit * storage =

(* booleans *)
      let bool =
        if parameter then true else false
      in
      let storage  = set storage 0 bool in

      ( (), storage )
