functor HecateLexFun(structure Tokens: Hecate_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT | INITIAL
    structure UserDeclarations = 
      struct

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun eof pos = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun inc r = (r := !r + 1)



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc lineNum; linePos := yypos :: !linePos; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TRAVERSAL(yypos, yypos + size yytext))
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.CASE(yypos, yypos + size yytext))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RECUR(yypos, yypos + size yytext))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ITERATE(yypos, yypos + size yytext))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LEFT(yypos, yypos + size yytext))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RIGHT(yypos, yypos + size yytext))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.EVAL(yypos, yypos + size yytext))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INTERFACE(yypos, yypos + size yytext))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INPUT(yypos, yypos + size yytext))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.OUTPUT(yypos, yypos + size yytext))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TINT(yypos, yypos + size yytext))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TBOOL(yypos, yypos + size yytext))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TSET(yypos, yypos + size yytext))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.CLASS(yypos, yypos + size yytext))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.CHILDREN(yypos, yypos + size yytext))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RULES(yypos, yypos + size yytext))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.SELF(yypos, yypos + size yytext))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ASSIGN(yypos, yypos + size yytext))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DOT(yypos, yypos + size yytext))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IF(yypos, yypos + size yytext))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.THEN(yypos, yypos + size yytext))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ELSE(yypos, yypos + size yytext))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.AND(yypos, yypos + size yytext))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.OR(yypos, yypos + size yytext))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NOT(yypos, yypos + size yytext))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.EQ(yypos, yypos + size yytext))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NEQ(yypos, yypos + size yytext))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LT(yypos, yypos + size yytext))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LE(yypos, yypos + size yytext))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.GT(yypos, yypos + size yytext))
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.GE(yypos, yypos + size yytext))
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.PLUS(yypos, yypos + size yytext))
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.MINUS(yypos, yypos + size yytext))
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TIMES(yypos, yypos + size yytext))
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DIVIDE(yypos, yypos + size yytext))
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.HOLE(yypos, yypos + size yytext))
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LBRACE(yypos, yypos + size yytext))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RBRACE(yypos, yypos + size yytext))
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LPAREN(yypos, yypos + size yytext))
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RPAREN(yypos, yypos + size yytext))
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LBRACK(yypos, yypos + size yytext))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RBRACK(yypos, yypos + size yytext))
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COLON(yypos, yypos + size yytext))
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.SEMICOLON(yypos, yypos + size yytext))
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COMMA(yypos, yypos + size yytext))
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BOOL(true, yypos, yypos + size yytext))
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BOOL(false, yypos, yypos + size yytext))
      end
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case Int.fromString yytext of
                NONE   => (ErrorMsg.error yypos ("illegal integer " ^ yytext); continue())
              | SOME n => Tokens.INT(n, yypos, yypos + size yytext))
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID(yytext, yypos, yypos + size yytext))
      end
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; continue()))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc lineNum; YYBEGIN INITIAL; continue()))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal character " ^ yytext); continue())
      end
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ40(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ46(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"`"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ52(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ51(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ50(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ48(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ47(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"a"
                  then yyQ44(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction22(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = #"`"
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ54(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ53(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ43(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"h"
                  then yyQ42(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = #"`"
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction18(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ58(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ57(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"l"
                  then yyQ56(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ55(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ64(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ63(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ62(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ67(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ66(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ65(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ70(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ69(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ68(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"f"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"f"
                  then if inp = #"e"
                      then yyQ59(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"i"
                  then yyQ60(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = #"v"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ61(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ75(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ74(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ73(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ72(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ71(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ78(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ77(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ76(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction5(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ86(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ85(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ84(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ83(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ82(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction9(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ94(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ93(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ92(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ91(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ90(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction12(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ89(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ96(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ95(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ88(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"p"
                  then yyQ87(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"g"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"g"
                  then if inp = #"f"
                      then yyQ79(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"n"
                  then yyQ80(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = #"u"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ81(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction48(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction48(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction48(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction48(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp = #"`"
              then yyAction48(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyAction48(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ100(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ99(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ98(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ97(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"`"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ104(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ103(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"`"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ106(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ105(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ102(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"l"
                  then yyQ101(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = #"`"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ112(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ111(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ110(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = #"`"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ118(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ117(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ116(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ115(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ114(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ113(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"`"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ120(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ119(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction50(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ109(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"h"
                  then yyQ108(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction13(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp = #"`"
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ123(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ122(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ121(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"?"
              then yyQ124(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ125(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ126(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ128(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ127(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ129(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ130(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < #"0"
              then yyAction49(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ130(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ130(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < #"0"
              then yyAction49(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ130(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ131 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ131(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ132 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ132(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ133 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ133(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ133(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ133(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ133(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ24(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"+"
                  then yyQ12(strm', lastMatch)
                else if inp < #"+"
                  then if inp = #"\""
                      then yyQ4(strm', lastMatch)
                    else if inp < #"\""
                      then if inp = #"\v"
                          then yyQ4(strm', lastMatch)
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ5(strm', lastMatch)
                            else if inp = #"\n"
                              then yyQ6(strm', lastMatch)
                              else yyQ4(strm', lastMatch)
                        else if inp = #" "
                          then yyQ5(strm', lastMatch)
                        else if inp = #"!"
                          then yyQ7(strm', lastMatch)
                          else yyQ4(strm', lastMatch)
                    else if inp = #"("
                      then yyQ9(strm', lastMatch)
                    else if inp < #"("
                      then if inp = #"&"
                          then yyQ8(strm', lastMatch)
                          else yyQ4(strm', lastMatch)
                    else if inp = #")"
                      then yyQ10(strm', lastMatch)
                      else yyQ11(strm', lastMatch)
                else if inp = #";"
                  then yyQ19(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"/"
                      then yyQ16(strm', lastMatch)
                    else if inp < #"/"
                      then if inp = #"-"
                          then yyQ14(strm', lastMatch)
                        else if inp = #","
                          then yyQ13(strm', lastMatch)
                          else yyQ15(strm', lastMatch)
                    else if inp = #":"
                      then yyQ18(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = #">"
                  then yyQ22(strm', lastMatch)
                else if inp < #">"
                  then if inp = #"<"
                      then yyQ20(strm', lastMatch)
                      else yyQ21(strm', lastMatch)
                else if inp = #"?"
                  then yyQ23(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = #"j"
              then yyQ24(strm', lastMatch)
            else if inp < #"j"
              then if inp = #"b"
                  then yyQ27(strm', lastMatch)
                else if inp < #"b"
                  then if inp = #"]"
                      then yyQ26(strm', lastMatch)
                    else if inp < #"]"
                      then if inp = #"["
                          then yyQ25(strm', lastMatch)
                        else if inp = #"\\"
                          then yyQ4(strm', lastMatch)
                          else yyQ24(strm', lastMatch)
                    else if inp = #"a"
                      then yyQ24(strm', lastMatch)
                      else yyQ4(strm', lastMatch)
                else if inp = #"f"
                  then yyQ30(strm', lastMatch)
                else if inp < #"f"
                  then if inp = #"d"
                      then yyQ24(strm', lastMatch)
                    else if inp = #"c"
                      then yyQ28(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #"i"
                  then yyQ31(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = #"s"
              then yyQ35(strm', lastMatch)
            else if inp < #"s"
              then if inp = #"o"
                  then yyQ33(strm', lastMatch)
                else if inp < #"o"
                  then if inp = #"l"
                      then yyQ32(strm', lastMatch)
                      else yyQ24(strm', lastMatch)
                else if inp = #"r"
                  then yyQ34(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = #"|"
              then yyQ38(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"u"
                  then yyQ24(strm', lastMatch)
                else if inp < #"u"
                  then yyQ36(strm', lastMatch)
                else if inp = #"{"
                  then yyQ37(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = #"}"
              then yyQ39(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ3(strm', lastMatch)
              else yyQ2(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COMMENT => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
