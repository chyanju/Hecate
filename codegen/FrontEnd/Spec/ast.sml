(* AST *)


(* Group (bundling one interface with classes) *)
structure Group = struct
  datatype t = T of {interface: Interface.t, classes: Class.t list}
  fun toString (T{interface, classes}) =
    Interface.toString interface ^ "\n" ^
    Prelude.join(map Class.toString classes) ^ "\n"
  fun mapInterface f (T{interface, classes}) = T{interface=f interface, classes=classes}
  fun mapClass f (T{interface, classes}) = T{interface=interface, classes=map f classes}
end


structure Spec = struct
  type t = Group.t list
  fun toString groups = Prelude.join(map Group.toString groups)
end

structure AST = struct
  type t = Spec.t * Schedule.t
  fun toString (spec, schedule) = Prelude.join([Spec.toString spec, Schedule.toString schedule])
end