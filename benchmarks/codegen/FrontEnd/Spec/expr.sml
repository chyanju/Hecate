(* Expr *)

structure Binop = struct
  datatype t = Plus | Minus | Times | Divide | And | Or
  fun toString Plus = "+"
    | toString Minus = "-"
    | toString Times = "*"
    | toString Divide = "/"
    | toString And = "&&"
    | toString Or = "||"
end

structure Binrel = struct
  datatype t = EQ | NEQ | GT | GE | LT | LE
  fun toString EQ = "=="
    | toString NEQ = "!="
    | toString GT = ">"
    | toString GE = ">="
    | toString LT = "<"
    | toString LE = "<="
end

structure Unop = struct
  datatype t = Not
  fun toString Not = "!"
end

structure Expr = struct
  datatype t =
      Null
    | Int of int
    | Bool of bool
    | Unop of Unop.t * t
    | Binop of Binop.t * t * t
    | Binrel of Binrel.t * t * t
    | If of t * t * t
    | Path of Path.t
    | Hack of int * Path.t * t
    | Call of string * (t list)

  fun toString Null = "null"
    | toString (Int n) = Int.toString n
    | toString (Bool b) = Bool.toString b
    | toString (Unop(oper, e)) = Format.format "%s(%s)" (map Format.STR [Unop.toString oper, toString e])
    | toString (Binop(oper, e1, e2)) = Format.format "(%s) %s (%s)" (map Format.STR [toString e1, Binop.toString oper, toString e2])
    | toString (Binrel(rel, e1, e2)) = Format.format "(%s) %s (%s)" (map Format.STR [toString e1, Binrel.toString rel, toString e2])
    | toString (If(ec, et, ef)) = Format.format "if (%s) then (%s) else (%s)" (map Format.STR (map toString [ec, et, ef]))
    | toString (Path p) = Path.toString p
    | toString (Hack(i, p, default)) = Format.format "[%s]{ %s : %s }" (map Format.STR [Int.toString i, Path.toString p, toString default])
    | toString (Call(f, [])) = f ^ "()"
    | toString (Call(f, es)) = f ^ "(" ^ Prelude.intercalate ", " (map toString es) ^ ")"
end