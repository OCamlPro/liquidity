
open Utils

let pp_print_paragraph ppf description =
  Format.fprintf ppf "@[%a@]"
    Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
    (split ' ' description)
