[%%version 0.14]

let%entry main
    (parameter : bool)
    (storage : bool)
  : bool * bool =

  parameter <> storage,
  parameter = storage