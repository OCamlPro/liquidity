class type ['a] t = object
  method ast : 'a
  method string : string
  method json : Ezjsonm.value
end

class type ['a] superposer_base = object
  method parse : string -> 'a
  method print : 'a -> string
  method encoding : 'a Json_encoding.encoding
end

class ['a] superposer :
  'a superposer_base -> object
    inherit ['a] superposer_base
    method ast : 'a -> 'a t
    method string : string -> 'a t
    method json : Ezjsonm.value -> 'a t
  end
