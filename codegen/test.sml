CM.make "sources.cm";

val args = CommandLine.arguments ();
val grammar_file : string = List.nth (args,0);
val schedule_file : string = List.nth (args,1);
val _ = Main.main("codegen", ["codegen", grammar_file, schedule_file]);
val _ = OS.Process.exit(OS.Process.success); 