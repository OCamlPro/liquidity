
type storage = {
  key : key;
  hash : string;
  }

let%entry main
          (parameter : signature)
          (storage : storage) =
  (*  let c = Contract.self () in *)
  let key_hash = Crypto.hash_key storage.key in
  if key_hash = tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx then
    Current.fail ();
  if key_hash =
    Crypto.hash_key edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV then
    Current.fail ();
  let delegate = Contract.manager (Current.source ()) in
  let spendable = Crypto.check storage.key parameter storage.hash in
  let amount = Current.amount () in
  let amount = match amount / 2p with
      None -> Current.fail () (* not possible *)
    | Some qr -> qr
  in
  let account_op, _account =
    Account.create
      key_hash delegate spendable
      (amount.(0) + amount.(1)) in
  let delegatable = false in
  let c_op, _c_addr =
    Contract.create
      key_hash delegate
      spendable delegatable
      amount.(0) 0
      (fun (parameter : unit) (storage : int) ->
         ([] : operation list), storage + 1
      )

  in
  ( [account_op; c_op], storage )
