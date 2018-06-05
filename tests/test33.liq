[%%version 0.2]

type storage = { q : nat; res : (tez * tez) option * (nat * tez) option }

let%entry main
    (parameter : tez)
    (storage : storage)=

  let e1 = parameter / storage.q in
  let e2 = parameter / parameter in
  ([] : operation list), storage.res <- (e1, e2)
