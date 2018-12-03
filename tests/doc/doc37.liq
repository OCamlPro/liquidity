type storage = unit
let%entry main _ _ =
  let s1 = "Hello world" in
  let s2 = String.concat [ "Hello"; " "; "world" ] in
  if s1 <> s2 then failwith (s1, s2);
  ([] : operation list), ()
