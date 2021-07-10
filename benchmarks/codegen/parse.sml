structure Parse : sig val parse : string -> unit  end =
struct 
  structure HecateLrVals = HecateLrValsFun(structure Token = LrParser.Token)
  structure Lex = HecateLexFun(structure Tokens = HecateLrVals.Tokens)
  structure HecateP = Join(structure ParserData = HecateLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
  fun parse filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	  val file = TextIO.openIn filename

	  fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
          fun get _ = TextIO.input file
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn, _) = HecateP.parse(30,lexer,parseerror,())
       in TextIO.closeIn file;
          absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error

end (* structure Parse *)



