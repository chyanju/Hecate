(* Traversal *)
structure TStmt = struct
  datatype t =
      SHole
    | SIterate of string * (t list)
    | SRecur of string

  fun toString SHole = "??;"
    | toString (SIterate(on, block)) = "iterate " ^ on ^ " { " ^ Prelude.unwords(map toString block) ^ " }"
    | toString (SRecur(on)) = "recur " ^ on ^ ";"
end

structure Case = struct
  type t = string * (TStmt.t list)
  fun toString (id, b) = "case " ^ id ^ " {\n" ^ Prelude.join(map TStmt.toString b) ^ "\n}"
end

structure Traversal = struct
  type t = string * (Case.t list)
  fun toString (id, cs) = "traversal " ^ id ^ " {\n" ^ (Prelude.join(map Case.toString cs)) ^ "\n}\n"
end