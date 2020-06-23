type storage = unit
let%entry default () _ =
  let s = Bytes.pack (1, 2, 3, 4) in
  let t = (Bytes.unpack s : (int * int * int * int) option) in
  begin match t with
    | None -> failwith "bad unpack"
    | Some t ->
      if t.(0) <> 1 then failwith "bad unpack"
  end;
  [], ()
