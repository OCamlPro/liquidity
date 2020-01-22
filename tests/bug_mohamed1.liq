[%%version 1.056]

(* A player's choice is either hidden: hashed value obtained from his/her choice
and an arbitrary (secret) nonce, or a revealed choice that may have value 0
(Rock), 1 (Paper), or 2 (Scissors)
*)
type move =
  | Hashed of bytes (* sha256 (move : bytes, nonce : bytes) *)
  | Revealed of int (* 0 = Rock | 1 = Paper | 2 = Scisors*)

(* A player's state, made of the address and the choice made *)
type player = {
  addr : address ;
  move : move
}

(* The storage of the contract is made of players' states, if they already
played, a timestamp indicating the date of the latest storage, and the value
of the current bet. *)
type storage = {
  player1 : player option ;
  player2 : player option ;
  last_update : timestamp ;
  bet : dun;
}

(* --- auxiliary functions --- *)

   (* People should reveal their choices within this time frame after both players
made their moves. Otherwise, they may be disqualified, and the person who
revealed wins. If there is no second player, the first one can also withdraw
his/her bet once this TTL expires. *)
let[@inline] time_to_live = 300 (*1200 (* 20 minutes *)*)

(* initial storage *)
let%init storage = {
  player1 = None;
  player2 = None;
  last_update = Current.time ();
  bet = 0DUN; (* correct bet will be fixed by the first player *)
}

(* initial storage as a function *)
let[@inline] empty_storage () = {
  player1 = None;
  player2 = None;
  last_update = Current.time ();
  bet = 0DUN;
}

(* Transfer the given amount to the given destination. Assumes a Unit contract
for the destination *)
let[@inline] make_transfer (dest : address) amount =
  Contract.call ~dest ~amount ()

(* Check that sha256 (0x (move ^ nonce) = commit. Fails otherwise *)
let[@inline] check_commitment move nonce commit =
  let s = Bytes.concat [move; nonce] in
  let h = Crypto.sha256 s in
  if h <> commit then
    failwith ("revelation does not match commitment. (move, nonce, s, hash(s), com) =",
              move, nonce, s, h, commit)

(* Decode the given choice. Returns 0 (Rock), 1 (Paper), or 2 (Scissors)
depending on the value of the move. Fails if input is different from 0x01, 0x02
and 0x03 *)
let[@inline] decode_move move =
  if move = 0x00 then 0
  else if move = 0x01 then 1
  else if move = 0x02 then 2
  else failwith "invalid move"

(* Check that Current.amount () is equal to the given amount. Fail otherwise *)
let[@inline] check_amount amnt =
  if (Current.amount ()) <> amnt then
    failwith ("Invalid amount. (expected, provided) = ", amnt, (Current.amount ()))

(* Check if given timestamp plus time_to_live is smaller Current timestamp
(timeout expired). Fail otherwise *)
let[@inline] check_timeouted (last_update : timestamp) =
  let diff = (Current.time()) - last_update in
  let timeout = time_to_live - diff in
  if timeout > 0 then
    failwith ("Cannot ask for refund yet. You should wait", timeout)


(* --- entry points --- *)

(* play: reveal a commitment of some choice. The second player should bet the
same amount of DUN than the first one (can be 0 DUN) *)
let%entry play commit storage =
  let play = Some { addr = Current.sender () ; move = Hashed commit } in
  let storage = match storage.player1 with
    | None -> (storage.player1 <- play).bet <- Current.amount ();
    | Some p ->
        if p.addr = Current.sender () then
          failwith "Cannot play twice with the same address";
        check_amount storage.bet;
        match storage.player2 with
        | None -> storage.player2 <- play
        | Some _ -> failwith "Two players already made their moves"
  in
  [], storage.last_update <- Current.time ()

(* Once the two players provided their commitments. They should reveal their
choices + nonces. The revelations are checked against commitment and saved in
the storage *)
let%entry reveal (move, nonce) storage =
  check_amount 0DUN;
  let is_player1, player =
    match storage.player1 with
    | None -> failwith "Not in a reveal phase"
    | Some p ->
        if p.addr = Current.sender () then true, p
        else
          match storage.player2 with
          | None -> failwith "Not in a reveal phase"
          | Some p ->
              if p.addr = Current.sender () then false, p
              else
                failwith "Only players can reveal"
  in
  match player.move with
  | Revealed _ -> failwith "Revelation already made"
  | Hashed commit ->
      check_commitment move nonce commit;
      let player = player.move <- Revealed (decode_move move) in
      let storage =
        if is_player1 then storage.player1 <- Some player
        else storage.player2 <- Some player
      in
      [], storage.last_update <- Current.time ()

(* finalize the game: refund the winner, or the player who played honestly
(ie. revealed). Indeed, players who do not reveal after "time_to_live" are
disqualified. *)

let%entry finalize () storage =
  check_amount 0DUN;
  let bet = storage.bet in
  let ops =
    match storage.player1 with
    | None -> failwith "Nothing to refund"
    | Some p1 ->
        match storage.player2 with
        | None ->
            check_timeouted storage.last_update;
            if Current.sender () <> p1.addr then failwith "Unauthorized caller";
            (* no other player after TTL, withdraw bet *)
            [ make_transfer p1.addr bet ]
        | Some p2 ->
            let p_caller, p_other =
              if Current.sender () = p1.addr then p1, p2
              else if Current.sender () = p2.addr then p2, p1
              else failwith "Unauthorized caller"
            in
            match p_caller.move with
            | Hashed _ -> failwith "You should reveal before asking for refund"
            | Revealed m1 ->
                match p_other.move with
                | Hashed _ ->
                    check_timeouted storage.last_update;
                    (* the player who didn't reveal after TTL will loose everything *)
                    [ make_transfer p_caller.addr (2p * bet) ]
                | Revealed m2 ->
                    (* Both revealed *)
                    let diff = m1 - m2 in
                    if diff = 0 then (* The two players made the same choice *)
                      [ make_transfer p_caller.addr bet;
                        make_transfer p_other.addr bet ]

                    else
                      (* Here the person who loses will get back "bet / 2", and
                      the winner will get "bet / 2". The goal is to incentivize
                      players to reveal their choices *)
                      let low, high = match bet / 2p with
                        | None -> failwith "invariant: unreachable"
                        | Some (res, rem) -> res, res + rem
                      in
                      if diff = 1 || diff = -2 then
                        [ make_transfer p_caller.addr (bet + high);
                          make_transfer p_other.addr low ]
                      else if diff = -1 || diff = 2 then
                        [ make_transfer p_other.addr (bet + high);
                          make_transfer p_caller.addr low ]
                      else failwith "invariant : unreachable"
  in ops, empty_storage ()
