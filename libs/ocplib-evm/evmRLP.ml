(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open EvmTypes

let encodeInt64 n =
  let s = Bytes.create 8 in
  EndianString.BigEndian.set_int64 s 0 n;
  s

let encodeInt64 n =
  if n = 0L then "\000"
  else
  let s = encodeInt64 n in
  let rec iter i s =
    if s.[i] = '\000' then iter (i+1) s
    else
      if i = 0 then s else
        String.sub s i (8-i)
  in
  iter 0 s

let encodeInt n = encodeInt64 (Int64.of_int n)

let decodeInt64 s =
  let len = String.length s in
  if len > 8 then -1L (* overflow, wait for Zarith *)
  else
    let s = String.make (8-len) '\000' ^ s in
    EndianString.BigEndian.get_int64 s 0


let z256 = Z.of_int 256

let decodeZ s =
  let len = String.length s in
  let rec iter i acc =
    if i = len then
      acc
    else
      let c = int_of_char s.[i] in
      let acc = Z.add (Z.mul acc z256) (Z.of_int c) in
      iter (i+1) acc
  in
  iter 0 Z.zero

let encode item =
  let b = Buffer.create 1000 in
  let rec encode b item =
    match item with
    | S str ->
       let len = String.length str in
       begin match len with
       | 0 ->
          Buffer.add_char b (char_of_int 0x80)
       | 1 ->
          let c = str.[0] in
          let i = int_of_char c in
          if i >= 128 then
            Buffer.add_char b (char_of_int 0x81);
       | _ ->
          if len <= 55 then begin
              Buffer.add_char b (char_of_int ( 0x80 + len));
            end
          else
            let header = encodeInt len in
            Buffer.add_char b (char_of_int ( 0xb7 + String.length header));
            Buffer.add_string b header;
       end;
       Buffer.add_string b str

    | L list ->
       let b' = Buffer.create 1000 in
       List.iter (encode b') list;
       let str = Buffer.contents b' in
       let len = String.length str in
       if len <= 55 then begin
           Buffer.add_char b (char_of_int (0xc0 + len));
         end else begin
            let header = encodeInt len in
            Buffer.add_char b (char_of_int ( 0xf7 + String.length header));
            Buffer.add_string b header;
         end;
       Buffer.add_string b str
  in
  encode b item;
  Buffer.contents b

let rec decode_partial s pos len =
  if pos >= len then failwith "EvmRLP.decode_partial";
  let c = s.[pos] in
  let i = int_of_char s.[pos] in
  if i < 0x80 then
    S (String.make 1 c), pos+1
  else
    if i = 0x80 then
      S "", pos+1
    else
      if i <= 0xb7 then
        let len = i - 0x80 in
        let str = String.sub s (pos+1) len in
        S str, pos+1+len
      else
        if i <= 0xbf then
          let hlen = i - 0xb7 in
          let h = String.make (8-hlen) '\000' ^ String.sub s (pos+1) hlen in
          let lenL = EndianString.BigEndian.get_int64 h 0 in
          let len = Int64.to_int lenL in
          let str = String.sub s (pos+1+hlen) len in
          S str, pos+1+hlen+len
        else
          if i <= 0xf7 then
            let len = i - 0xc0 in
            decode_list s (pos+1) (pos+1+len) []
          else
            let hlen = i - 0xf7 in
            let h = String.make (8-hlen) '\000' ^ String.sub s (pos+1) hlen in
            let lenL = EndianString.BigEndian.get_int64 h 0 in
            let len = Int64.to_int lenL in
            let pos = pos+1+hlen in
            decode_list s pos (pos+len) []


and decode_list s pos len revlist =
  if pos > len then failwith "EvmRLP.decode_list";
  if pos = len then
    L (List.rev revlist), pos
  else
    let item, pos = decode_partial s pos len in
    decode_list s pos len (item :: revlist)

let decode_partial s pos len =
  let item, pos = decode_partial s pos len in
  if pos > len then failwith "EvmRLP.decode_partial";
  item, pos

let decode s =
  let len = String.length s in
  let item, pos = decode_partial s 0 len in
  if pos <> len then failwith "EvmRLP.decode";
  assert (encode item = s);
  item

                  (*
let encode x =
  let y = encode x in
  Printf.eprintf "encode = %S\n%s%!" y (to_string x);
  assert (decode y = x);
  y
                   *)

let () =

  assert (encode (S "dog") = "\131dog");
  assert (encode (S "") = "\128");
  assert (encode (S " ") = " ");
  assert (encode (L [S  "cat"; S "dog"]) = "\200\131cat\131dog");
  assert (encode (L []) = "\192");
  assert (encode (S "\004\000") = "\130\004\000");
  assert (encode (L [ L []; L [ L []]; L [L []; L [L[]] ] ]) = "\199\192\193\192\195\192\193\192");
  assert (encode (S "Lorem ipsum dolor sit amet, consectetur adipisicing elit") = "\1848Lorem ipsum dolor sit amet, consectetur adipisicing elit");
  assert (encode (L [S "Lorem ipsum dolor sit amet, consectetur adipisicing elit"]) = "\248:\1848Lorem ipsum dolor sit amet, consectetur adipisicing elit");
  ()
