type storage = (bool -> unit -> int) * bytes

let g (x:unit) (x:unit) (x:unit) (x:unit) (x:unit) = 0

let%entry main (f : tez -> bool -> int) (s, b) =
  (* let g (x:unit) (x:unit) (x:unit) (x:unit) (x:unit) = 0 in *)
  (* let a = 9tz in *)
  let l = [] in
  let r b _ =
    let x = s b () in
    let y = f 9tz false in
    let z = g () () () () () in
    x + y + z in
  let b = if r true () = 0 then 0x else b in
  let l = r :: r :: l in
  [], (s, b)
