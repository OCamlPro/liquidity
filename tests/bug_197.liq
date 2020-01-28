contract Minimal = struct
  type storage = {
    admins : (string, address) map ;
    stuff : unit ;
  }

  let find (storage : storage) =
    match Map.find "blah" storage.admins with
    | None -> ()
    | Some _ -> ()

  let%entry blah (_ : unit) (storage : storage) =
    find storage;
    [], storage
end

type storage = unit

let%entry blah (_ : unit) (_ : storage) =
  [], ()
