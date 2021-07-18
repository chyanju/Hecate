structure IR_ResolveEval = struct
  (* IR Class *)
  structure IR_Class = struct

    exception Child
    exception Rule
    
    datatype t = T of {
      name: string,
      interface: string,
      children: CType.t StrHT.hash_table (* map string -> CType.t *),
      rules: Expr.t PathHT.hash_table (* map Path.t -> Expr.t *)
    }

    fun make (Class.T {name, interface, children, rules}) =
      let
        val sizeHint = 128
        val children' = StrHT.mkTable(sizeHint, Child)
        val rules' = PathHT.mkTable(sizeHint, Rule)
      in
        foldr (fn ((name,ctyp), ()) => StrHT.insert children' (name,ctyp)) () children;
        foldr (fn ((p,e), ()) => PathHT.insert rules' (p,e)) () rules;
        T {name=name, interface=interface, children=children', rules=rules'}
      end

    fun toString (T {name, interface, children, rules}) =
      Class.toString (Class.T {
        name=name,
        interface=interface,
        children=StrHT.listItemsi children,
        rules=PathHT.listItemsi rules
      })

    
  end (* IR_Class *)

  (* IR Schedule *)
  structure IR_SStmt = struct
    type dir = SStmt.dir

    datatype t =
        Eval of Path.t * Expr.t
      | Recur of string
      | Iterate of dir option * string * (t list) (* TODO: make string -> Path.Child *)

    fun toString (Eval(p,e)) = Path.toString p ^ " := " ^ Expr.toString e
      | toString (Iterate(NONE, on, b)) = "iterate " ^ on ^ " { " ^ Prelude.unwords(map toString b) ^ " }"
      | toString (Iterate(SOME(d), on, b)) = "iterate[" ^ SStmt.dirToString d ^ "] " ^ on ^ " { " ^ Prelude.unwords(map toString b) ^ " }"
      | toString (Recur(on)) = "recur " ^ on ^ ";"

    fun make (rules: Expr.t PathHT.hash_table) (s: SStmt.t) = case s of
        SStmt.Eval(p) => (case PathHT.find rules p of
          SOME e => Eval(p, e)
        | NONE => (print ("No such rule: " ^ Path.toString p); raise IR_Class.Rule))
      | SStmt.Recur(str) => Recur(str)
      | SStmt.Iterate(d, on, ss) => 
        Iterate(d, on, map (make rules) ss)
  end (* IR_SStmt *)

  structure IR_ScheduleCase = struct
    type t = string * (IR_SStmt.t list)
    
    fun toString (id, b) = "case " ^ id ^ " {\n" ^ Prelude.join(map IR_SStmt.toString b) ^ "\n}"

    fun make (rules: Expr.t PathHT.hash_table) (c as (name, ss): ScheduleCase.t) : t =
      (name, map (IR_SStmt.make rules) ss)
  end (* IR_ScheduleCase *)

  structure IR_Schedule = struct
    type t = string * (IR_ScheduleCase.t StrHT.hash_table)

    exception Case
    
    fun toString (sch as (id, ct): t) =
      Format.format "traversal %s {\n%s\n}\n" (map Format.STR [
        id,
        Prelude.join(map IR_ScheduleCase.toString (StrHT.listItems ct))
      ])

    fun make (classes: IR_Class.t StrHT.hash_table) (sch: Schedule.t) : t =
      let
        val (name, cases) = sch
        val sizeHint = 128
        val ct = StrHT.mkTable(sizeHint, Case)

      in app (fn (c as (case_name, _)) =>
        let
          val IR_Class.T{rules, ...} = StrHT.lookup classes case_name
          val ir_case = IR_ScheduleCase.make rules c
        in
          StrHT.insert ct (case_name, ir_case)
        end) cases;
        (name, ct)
      end
  end (* IR_Schedule *)

  structure IR_Interface = struct
    open Interface
    val make = Prelude.id
  end

  exception Interface
  exception Class
  
  datatype t = T of {
    interfaces: IR_Interface.t StrHT.hash_table,
    classes: IR_Class.t StrHT.hash_table,
    schedule: IR_Schedule.t
  }

  fun make (interfaces: Interface.t list) (classes: Class.t list) (sch: Schedule.t) : t =
    let
      val sizeHint = 128
      val ir_interfaces = StrHT.mkTable(sizeHint, Interface)
      val ir_classes = StrHT.mkTable(sizeHint, Class)
    in
      app (fn (interface as (name, _)) => StrHT.insert ir_interfaces (name, IR_Interface.make interface)) interfaces;
      app (fn (class as (Class.T{name, ...})) => StrHT.insert ir_classes (name, IR_Class.make class)) classes;
      T {
        interfaces=ir_interfaces,
        classes=ir_classes,
        schedule=IR_Schedule.make ir_classes sch
      }
    end

  fun toString (T{interfaces, classes, schedule}) =
    let
      val interfaces_strs = map IR_Interface.toString (StrHT.listItems interfaces)
      val classes_strs = map IR_Class.toString (StrHT.listItems classes)
      val schedule_str = IR_Schedule.toString schedule
    in
      Prelude.join(interfaces_strs @ classes_strs @ [schedule_str])
    end

end (* IR_AST *)