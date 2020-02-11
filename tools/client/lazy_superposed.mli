type 'a t

val ast : 'a -> 'a t
val json : Ezjsonm.value -> 'a t
val string : string -> 'a t

module Make : functor
  (C : sig
     type t
     val parse : string -> t
     val print : t -> string
     val encoding : t Json_encoding.encoding
   end) ->
sig
  val force_ast : C.t t -> C.t
  val force_string : C.t t -> string
  val force_json : C.t t -> Ezjsonm.value
end
