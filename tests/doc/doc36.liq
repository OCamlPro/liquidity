type storage = string
let%entry default () _ =
  let s = "Hello world" in
  let world = match String.sub 6p 5p s with
    | Some s -> s
    | None -> failwith () in
  [], world
