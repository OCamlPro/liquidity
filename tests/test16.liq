
let contract
      (parameter : int)
      (storage : unit)
      (return : unit) =
  let f = fun ( arg : unit * int ) ->
    arg.(0)
  in
  let storage = Lambda.pipe (storage, parameter) f in
  ( (), storage )

































