
type t = Num.num

let zero = Num.num_of_int 0

let of_int = Num.num_of_int
let to_int = Num.int_of_num
let add = Num.add_num
let mul = Num.mult_num

let div_rem a b =
  Num.quo_num a b, Num.mod_num a b

let of_bits b =
  let n = ref (Num.num_of_int 0) in
  let p = ref (Num.num_of_int 1) in
  let f = Num.num_of_int 256 in
  for i = 0 to String.length b - 1 do
    let c = Char.code (String.get b i) in
    n := Num.add_num !n (Num.mult_num !p (Num.num_of_int c));
    p := Num.mult_num !p f;
  done;
  !n

let to_bits n =
  let n = ref (Num.abs_num n) in
  let l = ref [] in
  let f = Num.num_of_int 256 in
  while Num.gt_num !n zero do
    let d, r = div_rem !n f in
    l := Char.unsafe_chr (to_int r) :: !l;
    n := d;
  done;
  let b = Bytes.create (List.length !l) in
  List.iteri (fun i c ->
      Bytes.set b i c
    ) (List.rev !l);
  Bytes.unsafe_to_string b

let compare = Num.compare_num

let numbits x = assert false

let sign = Num.sign_num

let neg = Num.minus_num

let equal = Num.eq_num

let extract _ _ _  =  assert false

let abs = Num.abs_num

let to_string = Num.string_of_num

let of_string = Num.num_of_string
