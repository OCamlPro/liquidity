
(* constants *)

let version = 1.0

let%entry main
      (parameter : bool)
      (storage :
        bool *
        int option *
        (string,int) map *
        int set *
        int list
      )
      [%return : unit] =

(* booleans *)
      let bool =
        if parameter then true else false
      in
      let storage  = set storage 0 bool in

      ( (), storage )
