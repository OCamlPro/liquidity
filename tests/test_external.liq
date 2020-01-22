

external ignore : [%stack: int] -> unit = "DROP" [@@effect]
external random : unit -> [%stack: int] = "RANDOM"
external swapp : [%type: 'a] -> [%type: 'b] ->
                 [%stack: 'a * 'b] -> [%stack: 'b * 'a] = "SWAPP"
external swap : [%type: 'a] -> [%type: 'b] ->
                  [%stack: 'a] -> [%stack: 'b] ->
                    [%stack: 'b] * [%stack: 'a] = "SWAP"
external many : unit -> [%stack: int] * [%stack: nat]
                        * [%stack: int] * [%stack: nat] = "MANY"

type storage = (nat * int) * (int * nat * int * nat)

let%entry default (parameter : nat) _storage =

  let _ = ignore 42 in
  let x = random () in
  let p = swapp [%type: nat] [%type: int] (parameter, x) in
  let q = swap [%type: int] [%type: nat] p.(0) p.(1) in
  let r = many () in

  ([], (q, r))
