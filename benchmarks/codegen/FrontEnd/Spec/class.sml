(* Class *)

structure CType = struct
  datatype t =
      Interface of string
    | List of string
  
  fun toString (Interface itf) = itf
    | toString (List itf) = "[" ^ itf ^ "]"
  
  fun toCpp (Interface itf) = itf ^ "*"
    | toCpp (List itf) = itf ^ "*"
end

structure Child = struct
  type t = string * CType.t;
  fun toString (id, ctyp) = id ^ " : " ^ CType.toString ctyp
end

structure Rule = struct
  type t = Path.t * Expr.t
  fun toString (p, e) = Path.toString p ^ " := " ^ Expr.toString e ^ ";"
end

structure Class = struct
  datatype t = T of {name: string, interface: string, children: Child.t list, rules: Rule.t list}
  fun toString (T{name, interface, children, rules}) =
    Format.format (Prelude.join [
      "class %s : %s {",
        "children {",
          "%s",
        "}",
        "rules {",
          "%s",
        "}",
      "}", ""])
      (map Format.STR [
        name,
        interface,
        Prelude.join(map Child.toString children),
        Prelude.join(map Rule.toString rules)])
  fun mapChild f (T{name, interface, children, rules}) =
    T{
      name=name,
      interface=interface,
      children=map f children,
      rules=rules}
  fun mapRule f (T{name, interface, children, rules}) =
    T{
      name=name,
      interface=interface,
      children=children,
      rules=map f rules}
end