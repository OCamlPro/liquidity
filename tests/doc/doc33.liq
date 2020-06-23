type storage = unit
let%entry default () _ =
  let b = 0x616161 in
  let s = Bytes.concat [ b; b ] in
  let b' = Bytes.sub 3p 3p s in
  begin match b' with
    | None -> failwith "Bad concat or sub !"
    | Some b' ->
        if b <> b' then failwith "Bad concat or sub !";
  end;
  [], ()
