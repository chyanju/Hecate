(* Schedule *)

structure SStmt = struct
  datatype dir = Left | Right
  fun dirToString Left = "left"
    | dirToString Right = "right"

  datatype t =
      Eval of Path.t
    | Recur of string
    | Iterate of dir option * string * (t list)

  fun toString (Eval p) = "eval " ^ Path.toString p
    | toString (Iterate(NONE, on, b)) = "iterate " ^ on ^ " { " ^ Prelude.unwords(map toString b) ^ " }"
    | toString (Iterate(SOME(d), on, b)) = "iterate[" ^ dirToString d ^ "] " ^ on ^ " { " ^ Prelude.unwords(map toString b) ^ " }"
    | toString (Recur(on)) = "recur " ^ on ^ ";"
end

structure ScheduleCase = struct
  type t = string * (SStmt.t list)
  fun toString (id, b) = "case " ^ id ^ " {\n" ^ Prelude.join(map SStmt.toString b) ^ "\n}"
end

structure Schedule = struct
  type t = string * (ScheduleCase.t list)
  fun toString (id, cs) = "traversal " ^ id ^ " {\n" ^ (Prelude.join(map ScheduleCase.toString cs)) ^ "\n}\n"
end