(* Interface *)

structure InOut = struct
  datatype t = I | O
  fun toString I = "input"
    | toString O = "output"
end

structure Type = struct
  datatype t = Int | Bool | Set | Custom of string | Vector of string
  fun toString Int = "int"
    | toString Bool = "bool"
    | toString Set = "set"
    | toString (Custom t) = t
    | toString (Vector t) = "vector<" ^ t ^ ">"
end

structure Field = struct
  type t = InOut.t * string * Type.t
  fun toString (io, id, t) = Format.format "%s %s : %s" (map Format.STR [InOut.toString io, id, Type.toString t])
end

structure Interface = struct
  type t = string * (Field.t list)
  fun toString (id, fs) = "interface " ^ id ^ " {\n" ^ (Prelude.join(map Field.toString fs)) ^ "\n}\n"
end