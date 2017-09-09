(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open EvmTypes

type db_key = string
type trie_key = string
type trie_value = string
type trie_node =
  | TrieNull
  | TrieBranch of db_key array * trie_value
  | TrieLeaf of string * trie_value
  | TrieExtension of string * db_key

let node_of_key db key =
  if key = "" then TrieNull
  else
    match LevelDB.get db key with
    | None -> TrieNull
    | Some s ->
       let rlp = EvmRLP.decode s in
       match rlp with
       | S "" -> assert false
       | L [ S hash0;
             S hash1;
             S hash2;
             S hash3;
             S hash4;
             S hash5;
             S hash6;
             S hash7;
             S hash8;
             S hash9;
             S hashA;
             S hashB;
             S hashC;
             S hashD;
             S hashE;
             S hashF;
             S value;
           ] ->
          TrieBranch ([|
                        hash0;
                        hash1;
                        hash2;
                        hash3;
                        hash4;
                        hash5;
                        hash6;
                        hash7;
                        hash8;
                        hash9;
                        hashA;
                        hashB;
                        hashC;
                        hashD;
                        hashE;
                        hashF;
                      |],
                      value)

       | L [ S encodedPath; S value ] ->
          let encodedPath = Hex_encode.hex_encode encodedPath in
          let switch_char = encodedPath.[0] in
          let is_extension =
            match  switch_char with
            | '0' -> true
            | '1' -> true
            | '2' -> false
            | '3' -> false
            | _ -> assert false
          in
          let len = String.length encodedPath in
          let encodedPath = match switch_char with
            | '0' | '2' -> String.sub encodedPath 2 (len-2)
            | '1' | '3' -> String.sub encodedPath 1 (len-1)
            | _ -> assert false
          in
          if is_extension then
            TrieExtension (encodedPath, value)
          else
            TrieLeaf (encodedPath, value)

       | _ -> assert false

let hexchars = "0123456789ABCDEF"
let int_of_hexchar c = match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a'|'A' -> 10
  | 'b'|'B' -> 11
  | 'c'|'C' -> 12
  | 'd'|'D' -> 13
  | 'e'|'E' -> 14
  | 'f'|'F' -> 15
  | _ -> assert false

let iter db root f =
  let rec trie_iter db hash prefix f =
    match node_of_key db hash with
    | TrieNull ->
       (* Printf.eprintf "Trie_Iter: no value for hash %S at %s\n%!"
            hash prefix; *)
       ()
    | TrieBranch (branches, value) ->
       if value <> "" then f prefix value;
       Array.iteri (fun i hash ->
           trie_iter db hash (prefix ^ String.make 1 hexchars.[i]) f) branches
    | TrieExtension (encodedPath, key) ->
       let prefix = prefix ^ encodedPath in
       trie_iter db key prefix f
    | TrieLeaf (encodedPath, value) ->
       let prefix = prefix ^ encodedPath in
       f prefix value
  in
  trie_iter db root ""
            (fun prefix value ->
              f (Hex_encode.hex_decode prefix) value)

let lookup db root hash =
  let prefix = Hex_encode.hex_encode hash in
  let rec lookup hash pos =
    (*    Printf.eprintf "lookup pos %d\n%!" pos; *)
    match node_of_key db hash with
    | TrieNull ->
       (*       Printf.eprintf "TrieNull\n%!"; *)
       None
    | TrieBranch (branches, value) ->
       (*       Printf.eprintf "TrieBranch\n%!"; *)
       if pos = 64 then Some value
       else
         let c = prefix.[pos] in
         let c = int_of_hexchar c in
         lookup branches.(c) (pos+1)
    | TrieLeaf (encodedPath, value) ->
       (*
       Printf.eprintf "TrieLeaf\n%!";
       Printf.eprintf "  Prefix: %s\n%!" prefix;
       Printf.eprintf "  encodedPath: %s\n%!" encodedPath;
        *)
       if String.sub prefix pos (64-pos) = encodedPath then
         Some value
       else
         None
    | TrieExtension (encodedPath, key) ->
       (*       Printf.eprintf "TrieExtension\n%!"; *)
       let len = String.length encodedPath in
       if String.sub prefix pos len = encodedPath then
         lookup key (pos+len)
       else
         None
  in
  lookup root 0
