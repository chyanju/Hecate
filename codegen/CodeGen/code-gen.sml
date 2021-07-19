structure Path_Cpp = struct
  open Path

  fun locCpp children Self = "this"
    | locCpp children (Child(c)) = case StringHashTable.lookup children c of
        CType.List _ => c (* iterator will have the same name as the list *)
      | _ => "this->" ^ c

  fun toCpp children ((l, f), _) = locCpp children l ^ "->" ^  f
end



structure Expr_Cpp = struct
  open Expr

  fun toCpp _ Null = "NULL_VAL"
  | toCpp _ (Bool true) = "true"
  | toCpp _ (Bool false) = "false"
  | toCpp _ (Int n) = Int.toString n
  | toCpp children (Unop(oper, e)) = Format.format "%s(%s)" (map Format.STR [Unop.toCpp oper, (toCpp children) e])
  | toCpp children (Binop(oper, e1, e2)) = Format.format "(%s) %s (%s)" (map Format.STR [toCpp children e1, Binop.toCpp oper, (toCpp children) e2])
  | toCpp children (Binrel(rel, e1, e2)) = Format.format "(%s) %s (%s)" (map Format.STR [toCpp children e1, Binrel.toCpp rel, (toCpp children) e2])
  | toCpp children (If(ec, et, ef)) = Format.format "(%s) ? (%s) : (%s)" (map Format.STR (map (toCpp children) [ec, et, ef]))
  | toCpp children (Path p) = Path_Cpp.toCpp children p
  | toCpp children (Hack(i, p, default)) = Format.format "(%s != NULL) ? (%s) : (%s)" (map Format.STR [Path_Cpp.locCpp children (Path.loc(p)), Path_Cpp.toCpp children p, (toCpp children) default])
  | toCpp children (Call(f, [])) = f ^ "()"
  | toCpp children (Call(f, es)) = f ^ "(" ^ Prelude.intercalate ", " (map (toCpp children) es) ^ ")"

  fun uninterpreted (Call(f, _)) = [f]
    | uninterpreted (Hack(_,_,e)) = uninterpreted e
    | uninterpreted (If(ec, et, ef)) = List.concat (List.map uninterpreted [ec, et, ef])
    | uninterpreted (Binrel(_, e1, e2)) = List.concat (List.map uninterpreted [e1, e2])
    | uninterpreted (Binop(_, e1, e2)) = List.concat (List.map uninterpreted [e1, e2])
    | uninterpreted (Unop(_, e)) = List.concat (List.map uninterpreted [e])
    | uninterpreted _ = []
end



structure IR_SStmt_Cpp = struct
  open IR_SortEval.IR_SStmt

  fun toCpp (children: CType.t StrHT.hash_table) s = case s of
      Eval(p,e) => Path_Cpp.toCpp children p ^ " = " ^ Expr_Cpp.toCpp children e ^ ";"
    | Iterate(_, on, ss) => (case StrHT.lookup children on of
        CType.Interface itf => Prelude.join(map (toCpp children) ss)
      | CType.List itf => Format.format "for (vector<%s>::iterator %s = this->%s.begin(); %s != this->%s.end(); %s++) {\n%s\n}" (map Format.STR [
          itf, (* iterator type *)
          on, (* iterator *)
          on, (* child *)
          on, (* iterator *)
          on, (* child *)
          on, (* iterator *)
          Prelude.join(map (toCpp children) ss)
      ])
      | CType.Option itf => 
        let val child = Path_Cpp.locCpp children (Path.Child(on))
        in
          Format.format "if (%s != NULL) {\n%s\n}" (map Format.STR [
            child,
            Prelude.join(map (toCpp children) ss)])
        end)
    | Recur(on) =>
      let val child = Path_Cpp.locCpp children (Path.Child(on))
      in (case StrHT.lookup children on of
        CType.Option itf => Format.format "if (%s != NULL) { %s->eval(); }" (map Format.STR [
          child,
          child])
      | _ => Format.format "%s->eval();" [Format.STR child])
      end
  
  fun uninterpreted (Eval(_, e)) = Expr_Cpp.uninterpreted e
    | uninterpreted (Iterate(_, _, ss)) = Prelude.concatMap uninterpreted ss
    | uninterpreted (Recur(_)) = []
end



structure Interface_Cpp = struct
  structure Prev = IR_SortEval
  val union = "Union"
  
  fun toCpp (interface as (name, fields): Prev.IR_Interface.t) : string list =
    let fun fieldToCpp (io, f, typ) =
        Format.format "%s %s; // %s" (map Format.STR [
          Type.toString typ,
          f,
          InOut.toString io])
    in
      [
        Format.format "class %s : public %s {" (map Format.STR [
          name, union
        ]),
        "public:"
      ]
      @ [Prelude.join(map fieldToCpp fields)]
      @ ["};\n"]
    end
end



structure ScheduleCase_Cpp = struct
  structure Prev = IR_SortEval

  fun toCpp children (plan as (_, ss): Prev.IR_ScheduleCase.t) : string list =
    ["void eval() override {"]
    @ map (IR_SStmt_Cpp.toCpp children) ss
    @ ["}\n"]
  
  fun uninterpreted (_, ss) = Prelude.concatMap IR_SStmt_Cpp.uninterpreted ss
end



structure Class_Cpp = struct

  structure Prev = IR_SortEval
  
  fun childrenToCpp (children: CType.t StrHT.hash_table) : string list =
    map (fn (child,ctyp) => CType.toCpp ctyp ^ " " ^ child ^ ";") (StrHT.listItemsi children)



  fun toCpp ((Prev.Prev.IR_Class.T{name, interface, children, rules}, plan): Prev.IR_Class.t) : string list =
    [
      Format.format "class %s : %s {" (map Format.STR [
        name, interface
      ]),
      "public:"
    ]
    @ [""]
    @ [Prelude.join(childrenToCpp children)]
    @ [""]
    @ ScheduleCase_Cpp.toCpp children plan
    @ ["};\n"]
  
  fun uninterpreted ((Prev.Prev.IR_Class.T{name, interface, children, rules}, plan): Prev.IR_Class.t) : string list =
    ScheduleCase_Cpp.uninterpreted plan
end


structure CodeGen = struct
  structure Prev = IR_SortEval

  val union = "Union"

  val unionCpp = [
    "class " ^ union ^ " {",
    "public:",
    "virtual void eval() = 0;",
    "};\n"
  ]

  fun stdErr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr)
  
  fun toCpp (Prev.T {interfaces, classes, schedule}) =
    let
      val interface_lst : Prev.IR_Interface.t list = StrHT.listItems interfaces
      val class_lst : Prev.IR_Class.t list = StrHT.listItems classes
      val uninterpreted_lst : string list = Prelude.concatMap Class_Cpp.uninterpreted (StrHT.listItems classes)
      val header = [
        "#include <vector>",
        "",
        "using namespace std;",
        "",
        "#define NULL_VAL 9999"
       ]
      val s1 = List.concat(map Interface_Cpp.toCpp interface_lst)
      val s2 = List.concat(map Class_Cpp.toCpp class_lst)
    in
      app (fn f => stdErr (Format.format "uninterpreted: %s\n" [Format.STR f])) uninterpreted_lst;
      Prelude.intercalate "\n\n" (map Prelude.join(
        [header, unionCpp, s1, s2]))
    end
end
