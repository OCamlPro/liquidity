type storage = unit
let%entry default () _ =
  let s = Bytes.pack (1, 2, 3, 4) in
  let t = Bytes.unpack s in
  begin match t with
    | None -> failwith "bad unpack"
    | Some (x, y, z, w) ->
      if x <> 1 || y <> 2 || z <> 3 || w <> 4
      then failwith "bad unpack";
  end;

  let b = Bytes.pack [1] in
  let l = Bytes.unpack b in
  begin match l with
    | None -> failwith "bad unpack"
    | Some l -> match l with
      | [] -> failwith "bad unpack"
      | x :: _ ->
        if x <> 1 then failwith "bad unpack"
  end;
  [], ()
