[%%version 0.33]

type storage = {
  key : key;
  hash : bytes;
  c : address;
}

let%init storage : storage = {
  key = 0x0085b1e4560f47f089d7b97aabcf46937a4c137a9c3f96f73f20c83621694e36d5;
  hash = 0xabcdef;
  c = KT1LLcCCB9Fr1hrkGzfdiJ9u3ZajbdckBFrF
}

(*
edsigtc766ouxRKCy6dfYMTanSB9NkaqnBk2eit2i1vNAcKAuMtQRZZUFWWcZ7NdhQgLkgXp9kYNKiXfKsBvpk6ebWwQkMbNvgk
*)

contract PlusOne = struct
  type storage = int

  let%init init_storage (x : bool) (y : int) = if x = false then 0 else y

  let%entry main (parameter : unit) (storage : storage) =
    ([] : operation list), storage + 1
end

contract type PS = contract type of PlusOne

let%entry main
    (parameter : signature)
    storage =
  (*  let c = Contract.self () in *)
  let key_hash = Crypto.hash_key storage.key in
  if key_hash = tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx then
    Current.failwith ();
  if key_hash =
     Crypto.hash_key edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV then
    Current.failwith ();
  let delegate = Some key_hash in
  let spendable = Crypto.check storage.key parameter storage.hash in
  let amount = Current.amount () in
  let amount = match amount / 2p with
      None -> Current.failwith () (* not possible *)
    | Some qr -> qr
  in
  let delegatable = false in
  let cocococ = (Contract.at storage.c : (contract PS) option) in
  let account_op, _account =
    Account.create
      key_hash delegate delegatable
      (amount.(0) + amount.(1)) in
  let c_op, c_addr =
    Contract.create
      ~manager:key_hash
      ~delegate
      ~spendable
      ~delegatable:true
      ~amount:amount.(0)
      ~storage:(PlusOne.init_storage true (9 : PlusOne.storage))
      (contract PlusOne)
      (* (contract struct
       *   type storage = int
       *   let%entry main (parameter : unit) (storage : storage) =
       *     ([] : operation list), storage + 1
       * end) *)
  in
  let storage = storage.c <- c_addr in
  ( [account_op; c_op], storage )
