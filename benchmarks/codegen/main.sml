CM.make "sources.cm";

open Parse;
val p = parse;
val args = CommandLine.arguments ();
val filename : string = List.nth (args,0);
p filename;
val _ = OS.Process.exit(OS.Process.success)