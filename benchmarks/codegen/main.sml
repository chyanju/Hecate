CM.make "sources.cm";

val args = CommandLine.arguments ();
val grammar_file : string = List.nth (args,0);
val schedule_file : string = List.nth (args,1);
val groups = Parse.parse grammar_file;
val schedule = ParseSchedule.parse schedule_file;
val _ = print (Prelude.join(map Group.toString groups));
val _ = print "\n";
val _ = print (Schedule.toString schedule);
val _ = print "\n";
val _ = OS.Process.exit(OS.Process.success);