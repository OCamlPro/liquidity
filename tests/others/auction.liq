(* Auction contract from https://www.michelson-lang.com/contract-a-day.html *)

[%%version 0.3]

type storage = {
  auction_end : timestamp;
  highest_bid : tez;
  bidder : key_hash;
}

let%entry main
    (parameter : key_hash)
    (storage : storage) =

  (* Check if auction has ended *)
  if Current.time () > storage.auction_end then Current.failwith ();
  
  let new_bid = Current.amount () in
  let new_bidder = parameter in
  (* Check if new bid is higher that the last *)
  if new_bid <= storage.highest_bid then Current.failwith ();

  let previous_bidder = storage.bidder in
  let previous_bid = storage.highest_bid in

  (* Set new highest bid in storage *)
  let storage = storage.highest_bid <- new_bid in
  let storage = storage.bidder <- new_bidder in

  (* refund previous bid to previous bidder *)
  let refund_to = Account.default previous_bidder in
  let op = Contract.call refund_to previous_bid () in
  ([op], storage)
