[%%version 0.4]

type storage = bool

let%init storage (i: int) (j: int) = i <= j

let%entry main (parameter : bool) storage  =
    let sto = ((11p && 10p) = 0p) || ((not parameter xor storage) && storage) in
    [], sto
