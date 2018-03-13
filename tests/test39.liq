[%%version 0.151]

let%init storage (i: int) (j: int) = i <= j

let%entry main (parameter : bool) (storage : bool) : bool * bool =
    ((11p && 10p) = 0p, (not parameter xor storage) && storage)
