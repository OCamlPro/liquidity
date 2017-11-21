
open Utils

let pp_print_paragraph ppf description =
  Format.fprintf ppf "@[%a@]"
    Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
    (split ' ' description)


let rec remove_elem_from_list nb = function
  | [] -> []
  | l when nb <= 0 -> l
  | _ :: tl -> remove_elem_from_list (nb - 1) tl


let take n l =
  let rec loop acc n = function
    | xs when n <= 0 -> Some (List.rev acc, xs)
    | [] -> None
    | x :: xs -> loop (x :: acc) (n-1) xs in
  loop [] n l

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if n >= x && String.sub s 0 x = prefix then
    Some (String.sub s x (n - x))
  else
    None
