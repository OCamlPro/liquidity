type 'a superposed =
  | AST of 'a
  | String of string
  | Json of Ezjsonm.value
  | ASTString of { ast : 'a; string : string }
  | ASTJson of { ast : 'a; json : Ezjsonm.value }
  | JsonString of { json : Ezjsonm.value; string : string }
  | All of { ast : 'a; json : Ezjsonm.value; string : string }

type 'a t = 'a superposed ref

let ast x = ref (AST x)
let json x = ref (Json x)
let string x = ref (String x)

module Make (C :  sig
    type t
    val parse : string -> t
    val print : t -> string
    val encoding : t Json_encoding.encoding
  end) = struct

  let force_ast x =
    match !x with
    | AST ast | ASTString { ast } | ASTJson { ast } | All { ast } -> ast
    | String string ->
      let ast = C.parse string in
      x := ASTString { ast; string };
      ast
    | Json json ->
      let ast = Json_encoding.destruct C.encoding json in
      x := ASTJson { ast; json };
      ast
    | JsonString { json; string } ->
      let ast = Json_encoding.destruct C.encoding json in
      x := All { ast; json; string };
      ast

  let force_string x =
    match !x with
    | String string | ASTString { string } | JsonString { string } | All { string } -> string
    | AST ast ->
      let string = C.print ast in
      x := ASTString { ast; string };
      string
    | Json json ->
      let ast = Json_encoding.destruct C.encoding json in
      let string = C.print ast in
      x := All { ast; json; string };
      string
    | ASTJson { ast; json } ->
      let string = C.print ast in
      x := All { ast; json; string };
      string

  let force_json x =
    match !x with
    | Json json | ASTJson { json } | JsonString { json } | All { json } -> json
    | AST ast ->
      let json = Json_encoding.construct C.encoding ast in
      x := ASTJson { ast; json };
      json
    | String string ->
      let ast = C.parse string in
      let json = Json_encoding.construct C.encoding ast in
      x := All { ast; json; string };
      json
    | ASTString { ast; string } ->
      let json = Json_encoding.construct C.encoding ast in
      x := All { ast; json; string };
      json

end
