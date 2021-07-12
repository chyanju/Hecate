(* Path *)

signature PATH =
sig
  exception Path

  datatype location = Self | Child of string
  type t
  type field = string
  val new: location * field -> t
  val loc: t -> location
  val field: t -> field 
  val toString : t -> string
  val locCpp: location -> string
  val toCpp: t -> string
  structure Table : MONO_HASH_TABLE where
    type Key.hash_key = t
end

structure Path :> PATH = struct
  exception Path
  
  datatype location = Self | Child of string
  type field = string
  type t = (location * field) * word (* location.field, plus cached hash value *)

  fun toString' (Self, f) = "self" ^ "." ^ f
    | toString' (Child(c), f) = c ^ "." ^ f

  fun toString (p,_) = toString' p

  fun locCpp Self = "this"
    | locCpp (Child(c)) = "this->" ^ c

  fun toCpp ((Self, f), _) = "this->" ^ f
    | toCpp ((Child(c), f), _) = "this->" ^ c ^ "->" ^ f

  fun new (p as (loc, f)) =
    (p, HashString.hashString (toString' p))
  
  fun loc ((l,_),_) = l
  
  fun field ((_,f),_) = f

  structure Table :> MONO_HASH_TABLE where
    type Key.hash_key = t = HashTableFn(
      type hash_key = t
      fun hashVal (_,h) = h
      fun sameKey ((_,h1),(_,h2)) = h1 = h2
    )
  
end