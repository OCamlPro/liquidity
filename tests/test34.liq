[%%version 0.15]

let%entry main
    (parameter : bool)
    (storage : bool)
  : bool * bool =

  parameter <> storage,
  parameter = storage