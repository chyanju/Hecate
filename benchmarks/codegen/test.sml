CM.make "sources.cm";

val args = CommandLine.arguments ();
val grammar_file : string = List.nth (args,0);
val schedule_file : string = List.nth (args,1);
val groups = Parse.parse grammar_file;
val groups' = map (Group.mapClass (Class.mapRule (fn (p,e) => (p, Optimize.foldIf e)))) groups;
val schedule = ParseSchedule.parse schedule_file;
val ast: AST.t = (groups', schedule);
val interfaces = map (fn (Group.T{interface, ...}) => interface) groups;
val classes = List.concat (map (fn (Group.T{classes, ...}) => classes) groups);
val ir_ast = IR_ResolveEval.make interfaces classes schedule;
val ir_ast = IR_SortEval.make ir_ast;
(* val _ = print(IR_SortEval.toString ir_ast); *)
(* val _ = print(CodeGen.toCpp ir_ast); *)
val _ = OS.Process.exit(OS.Process.success);