
let%entry main
      (parameter : int)
      (storage : unit) =
  let f = fun ( arg : unit * int ) ->
    arg.(0)
  in
  let storage = Lambda.pipe (storage, parameter) f in
  ( ([] : operation list), storage )

































