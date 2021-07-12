structure Main = struct

  fun main (prog_name, args) = let
    val grammar_file : string = List.nth (args,1)
    val schedule_file : string = List.nth (args,2)
    val groups = Parse.parse grammar_file
    val groups' = map (Group.mapClass (Class.mapRule (fn (p,e) => (p, Optimize.foldIf e)))) groups
    val schedule = ParseSchedule.parse schedule_file
    val ast: AST.t = (groups', schedule)
    val interfaces = map (fn (Group.T{interface, ...}) => interface) groups
    val classes = List.concat (map (fn (Group.T{classes, ...}) => classes) groups)
    val ir_ast = IR_ResolveEval.make interfaces classes schedule
    val ir_ast = IR_SortEval.make ir_ast
  in
    (* print(grammar_file ^ "\n");
    print(schedule_file ^ "\n"); *)
    print(CodeGen.toCpp ir_ast);
    1
  end

end