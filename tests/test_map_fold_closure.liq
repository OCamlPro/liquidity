[%%version 0.3]

type storage = (bool * (string, bool) map)

let%entry main
  (parameter : (string, tez) map)
  (storage) =

  let amount = Current.amount() in
  let f (arg: (string * tez)) =
    arg.(1) + amount > 5.0tz
  in

  let is_ok (t:tez) = t <= 5tz in

  let m, ok = Map.map_fold (fun ((x : (string * tez)), (acc : bool)) ->
      (f x, acc && is_ok x.(1))
    ) parameter true in
  (([] : operation list), (ok, m))
