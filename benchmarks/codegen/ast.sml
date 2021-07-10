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

structure Block = struct
  type t = TStmt.t list
  fun toString b = Prelude.join(map TStmt.toString b)
end

structure Case = struct
  type t = string * Block.t
  fun toString (id, b) = "case " ^ id ^ " {\n" ^ Block.toString b ^ "\n}"
end

structure Traversal = struct
  type t = string * (Case.t list)
  fun toString (id, cs) = "traversal " ^ id ^ " {\n" ^ (Prelude.join(map Case.toString cs)) ^ "\n}\n"
end

(* Interface *)
structure InOut = struct
  datatype t = I | O
  fun toString I = "input"
    | toString O = "output"
end

structure Type = struct
  datatype t = Int | Bool | Set
  fun toString Int = "int"
    | toString Bool = "bool"
    | toString Set = "set"
end

structure Field = struct
  type t = InOut.t * string * Type.t
  fun toString (io, id, t) = Format.format "%s %s : %s" (map Format.STR [InOut.toString io, id, Type.toString t])
end

structure Interface = struct
  type t = string * (Field.t list)
  fun toString (id, fs) = "interface " ^ id ^ " {\n" ^ (Prelude.join(map Field.toString fs)) ^ "\n}\n"
end

structure CType = struct
  datatype t =
      Interface of string
    | List of string
  fun toString (Interface itf) = itf
    | toString (List itf) = "[" ^ itf ^ "]"
end

(* Class *)
structure Child = struct
  type t = string * CType.t;
  fun toString (id, ctyp) = id ^ " : " ^ CType.toString ctyp
end

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

structure Path = struct
  datatype loc = Self | Child of string
  datatype t = Dot of loc * string
  fun toString (Dot(Self, f)) = "self" ^ "." ^ f
    | toString (Dot(Child(c), f)) = c ^ "." ^ f
end

structure Expr = struct
  datatype t =
      Int of int
    | Bool of bool
    | Unop of Unop.t * t
    | Binop of Binop.t * t * t
    | Binrel of Binrel.t * t * t
    | If of t * t * t
    | Path of Path.t
    | Hack of int * Path.t * t
    | Call of string * (t list)

  fun toString (Int n) = Int.toString n
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

structure Rule = struct
  type t = Path.t * Expr.t
  fun toString (p, e) = Path.toString p ^ " := " ^ Expr.toString e ^ ";"
end

structure Class = struct
  type t = {name: string, interface: string, children: Child.t list, rules: Rule.t list}
  fun toString {name, interface, children, rules} =
    Format.format "class %s : %s {\nchildren {\n%s\n}\nrules {\n%s\n}\n}\n" (map Format.STR [
      name,
      interface,
      Prelude.join(map Child.toString children),
      Prelude.join(map Rule.toString rules)])
end

structure AST = struct
  type pos = int
  type symbol = Symbol.symbol
end