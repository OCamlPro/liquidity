

type _ kind =
  Input : in_channel kind
| Output : out_channel kind

let with_file : type a. mode:a kind -> string ->
                             (a -> 'b) ->
                             'b
  = fun ~mode file f ->
  match mode, f with
  | Input, f ->
     let ic = open_in_bin file in
     let x = f ic in
     close_in ic;
     x
  | Output, f ->
     let oc = open_out_bin file in
     let x = f oc in
     close_out oc;
     x

let write oc str = output_string oc str; Lwt.return ()
let read ic =
  let s  = Bytes.create 64_000 in
  let len = input ic s 0 (String.length s) in
  Lwt.return (String.sub s 0 len)
