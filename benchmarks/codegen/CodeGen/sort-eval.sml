structure IR_SortEval = struct
  
  structure StrHT = StringHashTable
  structure PathHT = Path.Table
  structure Prev = IR_ResolveEval (* previous phase *)

  type 'a ht = 'a StrHT.hash_table

  structure IR_Class = struct 
    type t =
      Prev.IR_Class.t (* {name, interface, children, rules *)
      * Prev.IR_ScheduleCase.t (* string * (Prev.IR_SStmt.t list) *)

    exception Class
    
    fun make
      (classes: Prev.IR_Class.t StrHT.hash_table)
      ((_, cases): Prev.IR_Schedule.t)
      : t StrHT.hash_table =
      let
        val sizeHint = 128
        val t = StrHT.mkTable(sizeHint, Class)
      in
        StrHT.appi (fn (class_name, class) => 
          let val plan = StrHT.lookup cases class_name
          in StrHT.insert t (class_name, (class, plan))
          end) classes;
        t
      end
    
    fun toString ((class, plan) : t) =
      Prev.IR_Class.toString class ^
      "// plan\n" ^ Prev.IR_ScheduleCase.toString plan ^ "\n"
  end

  structure IR_Interface = Prev.IR_Interface
  structure IR_Schedule = Prev.IR_Schedule

  datatype t = T of {
    interfaces: IR_Interface.t StrHT.hash_table,
    classes: IR_Class.t StrHT.hash_table,
    schedule: IR_Schedule.t
  }

  fun make (Prev.T {interfaces, classes, schedule}) : t =
    T {
      interfaces=interfaces,
      classes=IR_Class.make classes schedule,
      schedule=schedule
    }
  
  fun toString (T {interfaces, classes, schedule}) =
    let
      val interfaces_strs = map IR_Interface.toString (StrHT.listItems interfaces)
      val classes_strs = map IR_Class.toString (StrHT.listItems classes)
      (* val schedule_str = IR_Schedule.toString schedule *)
    in
      Prelude.join(
        interfaces_strs 
        @ classes_strs 
        (* @ [schedule_str] *)
      )
    end
end
      
    