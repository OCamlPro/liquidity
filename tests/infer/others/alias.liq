type kind =
  | Official
  | Contrib of address
  | Default
type alias = {
  alias : string ;
  avatar : bytes option;
  fees : tez ;
  kind : kind ;
  date : timestamp ;
}
type alias_map = (address, alias) big_map
type alias_operation = address * string * bytes option
type storage = {
  map : alias_map;
  min_fees : tez;
  owners : address set;
  update : ( alias_operation -> operation ) option;
}

let%init storage = {
  map = BigMap;
  min_fees = 0tz;
  owners = Set [ ( tz1ZLyEcFfXyu75ASsjAt5hgaR4sU5PSbLkg : address )  ];
  update = None;
}

let return = [] [@@inline]

let sender = Current.sender () [@@inline]

let%entry set_alias alias_operation storage =
  let owned = Set.mem sender storage.owners in
  match storage.update with
  | Some lambda ->
    ( [lambda alias_operation], storage )
  | None ->
    let (address, alias, avatar ) = alias_operation in
    let amount = Current.amount () in
    if amount < storage.min_fees && not owned then
      failwith ("bad amount, minimum = ", storage.min_fees)
    else
      let alias = {
        alias;
        avatar;
        fees = amount;
        kind = (if address = sender then
                  Official
                else
                if owned then Default
                else Contrib sender);
        date = Current.time ();
      } in
      begin
        match Map.find address storage.map with
        | None -> ()
        | Some old_alias ->
          match old_alias.kind with
          | Official ->
            if sender = address then ()
            else
              failwith "can't change official alias"
          | Default -> ()
          | Contrib contributor ->
            if sender <> address &&
               sender <> contributor &&
               old_alias.fees >= amount &&
               not owned then
              failwith ( "Bad amount, minimum = ", old_alias.fees )
      end;
      let storage = storage.map <- Map.add address alias storage.map in
      ( return , storage )

let%entry set_fees min_fees storage =
  let owned = Set.mem sender storage.owners in
  if owned then
    let storage = storage.min_fees <- min_fees in
    ( return, storage )
  else failwith "forbidden"

let%entry set_owner param storage =
  let address, add = param in
  let owned = Set.mem sender storage.owners in
  if owned then
    let storage = storage.owners <-
        (if add then
           Set.add address storage.owners
         else
         if Set.size storage.owners = 1p then
           failwith "cannot remove last owner"
         else
           Set.remove address storage.owners)
    in
    ( return , storage )
  else failwith "forbidden"

let%entry set_update lambda storage =
  let owned = Set.mem sender storage.owners in
  if owned then
    let storage = storage.update <- Some lambda in
    ( return, storage )
  else failwith "forbidden"
