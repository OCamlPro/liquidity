[%%version 0.2]

let%init storage (i: int) (j: int) = i <= j

let%entry main (parameter : bool) (storage : bool)  =
    let sto = ((11p && 10p) = 0p) || ((not parameter xor storage) && storage) in
    ([] : operation list), sto
