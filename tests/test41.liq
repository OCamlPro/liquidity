type storage = string

let%entry default (parameter : string) storage =
  let size = String.size parameter in
  let size_minus_one = match%nat size-1 with
    | Minus _ -> failwith "Wrong size"
    | Plus n -> n
  in
   let storage = match String.slice 0p size_minus_one parameter with
       | None -> storage
       | Some s -> String.concat [s; storage]
   in
   [], storage
