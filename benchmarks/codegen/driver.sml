structure Parse =
struct 
  structure Lex = Mlex

  fun parse filename =
      let val file = TextIO.openIn filename
          fun get _ = TextIO.input file
	  val lexer = Lex.makeLexer get
	  fun do_it() =
	      let val t = lexer()
	       in print t; print "\n";
		   if substring(t,0,3)="EOF" then () else do_it()
	      end
       in ErrorMsg.fileName := filename;
          do_it();
	  TextIO.closeIn file
      end

end

