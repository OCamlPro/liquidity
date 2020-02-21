
type game = {
  number : nat;
  bet : tez;
  player : key_hash;
}

type storage = {
  game : game option;
  oracle_id : address;
}

let%init storage (oracle_id : address) =
  { game = None; oracle_id }

(* Start a new game *)
let%entry play ((number : nat), (player : key_hash)) storage =
  if number > 100p then failwith "number must be <= 100";
  if Current.amount () = 0tz then failwith "bet cannot be 0tz";
  if 2p * Current.amount () > Current.balance () then
    failwith "I don't have enough money for this bet";
  match storage.game with
  | Some g ->
    failwith ("Game already started with", g)
  | None ->
    let bet = Current.amount () in
    let storage = storage.game <- Some { number; bet; player } in
    ([], storage)

(* Receive a random number from the oracle and compute outcome of the
   game *)
let%entry finish (random_number : nat) storage =
  let random_number = match random_number / 101p with
    | None -> failwith ()
    | Some (_, r) -> r in
  if Current.sender () <> storage.oracle_id then
    failwith ("Random numbers cannot be generated");
  match storage.game with
  | None -> failwith "No game already started"
  | Some game ->
    let ops =
      if random_number < game.number then
        (* Lose *)
        []
      else
        (* Win *)
        let gain = match (game.bet * game.number / 100p) with
          | None -> 0tz
          | Some (g, _) -> g in
        let reimbursed = game.bet + gain in
        [ Account.transfer ~dest:game.player ~amount:reimbursed ]
    in
    let storage = storage.game <- None in
    (ops, storage)

(* accept funds *)
let%entry default () storage =
  [], storage
