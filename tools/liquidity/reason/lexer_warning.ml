let warn_latin1 lexbuf =
  let loc = Location.curr lexbuf in
  Location.prerr_warning loc
  (Warnings.Deprecated ("ISO-Latin1 characters in identifiers",loc,loc) )
;;
