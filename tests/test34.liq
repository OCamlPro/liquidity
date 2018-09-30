[%%version 0.3]

let%entry main
    (parameter : bool)
    (storage : bool) =

  ([] : operation list),
  (parameter <> storage)
  &&
  (parameter = storage)