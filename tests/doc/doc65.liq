type storage = (string, int) map
let%entry default () map =
  let negated_values, min_key = Map.map_fold (fun ((key, v) , min_key) ->
      let min_key = match min_key with
        | None -> Some key
        | Some min -> if key < min then Some key else min_key in
      ( - v, min_key )
    ) map None
  in
  [], negated_values
