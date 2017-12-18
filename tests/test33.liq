[%%version 0.14]

let%entry main
    (parameter : tez)
    (storage : nat)
  : ((tez * tez) option * (nat * tez) option) * nat =

  let e1 = parameter / storage in
  let e2 = parameter / parameter in
  (e1, e2), storage
