
val contract_ast : Parsetree.structure -> string
val string_of_structure :
  Parsetree.structure -> (string * Location.t) list -> string
val string_of_expression : Parsetree.expression -> string
