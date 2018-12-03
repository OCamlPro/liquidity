type storage = (string, int) map
let%entry main _ map =
  let negated_values, min_key = Map.map_fold (fun ((key, v) , min_key) ->
      let min_key = match min_key with
        | None -> Some key
        | Some min -> if key < min then Some key else min_key in
      ( - v, min_key )
    ) map (None : string option)
  in
  ([] : operation list), negated_values
