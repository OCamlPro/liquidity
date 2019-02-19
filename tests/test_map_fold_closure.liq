[%%version 0.5]

type storage = (bool * (string, bool) map)

let%entry main
  (parameter : (string, tez) map)
  _storage =

  let amount = Current.amount() in
  let f (_, acc) =
    acc + amount > 5.0tz
  in

  let is_ok t = t <= 5tz in

  let m, ok = Map.map_fold (fun (x, acc) ->
      (f x, acc && is_ok x.(1))
    ) parameter true in
  ([], (ok, m))
