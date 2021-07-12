structure CodeGen = struct
  structure Prev = IR_SortEval
  structure StrHT = Prev.StrHT

  val union = "Union"

  val unionCpp = [
    "class " ^ union ^ " {",
    "public:",
    "virtual void eval() = 0;",
    "};\n"
  ]

  fun interfaceToCpp (interface as (name, fields): Prev.IR_Interface.t) : string list =
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
      @ map fieldToCpp fields
      @ ["};\n"]
    end
  
  fun planToCpp (plan as (_, ss): Prev.Prev.IR_ScheduleCase.t) : string list =
    ["void eval() override {"]
    @ map Prev.Prev.IR_SStmt.toCpp ss
    @ ["}\n"]

  fun childrenToCpp (children: CType.t StrHT.hash_table) : string list =
    map (fn (child,ctyp) => CType.toCpp ctyp ^ " " ^ child ^ ";") (StrHT.listItemsi children)
  
  fun classToCpp ((Prev.Prev.IR_Class.T{name, interface, children, rules}, plan): Prev.IR_Class.t) : string list =
    [
      Format.format "class %s : %s {" (map Format.STR [
        name, interface
      ]),
      "public:"
    ]
    @ [""]
    @ childrenToCpp children
    @ [""]
    @ planToCpp plan
    @ ["};\n"]

  fun toCpp (Prev.T {interfaces, classes, schedule}) =
    let
      val interface_lst : Prev.IR_Interface.t list = StrHT.listItems interfaces
      val class_lst : Prev.IR_Class.t list = StrHT.listItems classes
      val s0 = ["#define NULL_VAL 9999"] @ [""] @ unionCpp
      val s1 = List.concat(map interfaceToCpp interface_lst)
      val s2 = List.concat(map classToCpp class_lst)
    in
      Prelude.join(s0 @ s1 @ s2)
    end
end
