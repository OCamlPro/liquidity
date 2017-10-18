
type storage = {
  key : key;
  hash : string;
  }

let%entry main
          (parameter : signature)
          (storage : storage)
    :  ( (unit,unit) contract * (unit,int) contract) *
         storage =
  (*  let c = Current.contract () in *)
  let key_hash = Crypto.hash_key storage.key in
  if key_hash = tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx then
    Current.fail ();
    if key_hash =
    Crypto.hash_key edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV then
    Current.fail ();
  let manager = Contract.manager (Source : (unit,unit) contract) in
  let spendable = Crypto.check storage.key (parameter, storage.hash) in
  let amount = Current.amount () in
  let amount = match amount / 2p with
      None -> Current.fail () (* not possible *)
    | Some qr -> qr
  in
  let account = Account.create
                  key_hash (Some manager) spendable
                  (amount.(0) + amount.(1)) in
  let delegatable = false in
  let c = Contract.create
            key_hash (Some manager)
            spendable delegatable
            amount.(0)
            (fun (x : unit * int) ->
              let n = x.(1) in
              n, n+1
            )
            0
  in
  ( (account,c), storage )
