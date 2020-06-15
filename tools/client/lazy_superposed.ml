type 'a superposed =
  | AST of 'a
  | String of string
  | Json of Ezjsonm.value
  | ASTString of { ast : 'a; string : string }
  | ASTJson of { ast : 'a; json : Ezjsonm.value }
  | JsonString of { json : Ezjsonm.value; string : string }
  | All of { ast : 'a; json : Ezjsonm.value; string : string }

class type ['a] superposer_base = object
  method parse : string -> 'a
  method print : 'a -> string
  method encoding : 'a Json_encoding.encoding
end

class ['a] t (s : 'a superposer_base) x = object

  val mutable x = x

  method ast =
    match x with
    | AST ast | ASTString { ast } | ASTJson { ast } | All { ast } -> ast
    | String string ->
      let ast = s#parse string in
      x <- ASTString { ast; string };
      ast
    | Json json ->
      let ast = Json_encoding.destruct s#encoding json in
      x <- ASTJson { ast; json };
      ast
    | JsonString { json; string } ->
      let ast = Json_encoding.destruct s#encoding json in
      x <- All { ast; json; string };
      ast

  method string =
    match x with
    | String string | ASTString { string } | JsonString { string } | All { string } -> string
    | AST ast ->
      let string = s#print ast in
      x <- ASTString { ast; string };
      string
    | Json json ->
      let ast = Json_encoding.destruct s#encoding json in
      let string = s#print ast in
      x <- All { ast; json; string };
      string
    | ASTJson { ast; json } ->
      let string = s#print ast in
      x <- All { ast; json; string };
      string

  method json =
    match x with
    | Json json | ASTJson { json } | JsonString { json } | All { json } -> json
    | AST ast ->
      let json = Json_encoding.construct s#encoding ast in
      x <- ASTJson { ast; json };
      json
    | String string ->
      let ast = s#parse string in
      let json = Json_encoding.construct s#encoding ast in
      x <- All { ast; json; string };
      json
    | ASTString { ast; string } ->
      let json = Json_encoding.construct s#encoding ast in
      x <- All { ast; json; string };
      json

end

class ['a] superposer (s : 'a superposer_base) = object
  method print = s#print
  method parse = s#parse
  method encoding = s#encoding
  method ast x = new t s (AST x)
  method string x = new t s (String x)
  method json x = new t s (Json x)
end
