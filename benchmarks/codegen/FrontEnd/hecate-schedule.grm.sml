functor HecateScheduleLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : HecateSchedule_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\003\000\000\000\
\\001\000\006\000\030\000\007\000\029\000\000\000\
\\001\000\019\000\020\000\048\000\019\000\000\000\
\\001\000\021\000\026\000\000\000\
\\001\000\021\000\027\000\000\000\
\\001\000\040\000\022\000\048\000\021\000\000\000\
\\001\000\041\000\035\000\000\000\
\\001\000\041\000\036\000\000\000\
\\001\000\042\000\005\000\000\000\
\\001\000\042\000\010\000\000\000\
\\001\000\042\000\028\000\000\000\
\\001\000\042\000\040\000\000\000\
\\001\000\042\000\041\000\000\000\
\\001\000\043\000\008\000\000\000\
\\001\000\043\000\017\000\000\000\
\\001\000\043\000\037\000\000\000\
\\001\000\043\000\044\000\000\000\
\\001\000\043\000\045\000\000\000\
\\001\000\045\000\025\000\000\000\
\\001\000\045\000\031\000\000\000\
\\001\000\048\000\004\000\000\000\
\\001\000\048\000\009\000\000\000\
\\001\000\048\000\023\000\000\000\
\\001\000\048\000\032\000\000\000\
\\001\000\048\000\033\000\000\000\
\\001\000\048\000\038\000\000\000\
\\001\000\048\000\039\000\000\000\
\\047\000\000\000\
\\048\000\003\000\007\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\004\000\015\000\005\000\014\000\008\000\013\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\000\000\
\"
val actionRowNumbers =
"\001\000\021\000\009\000\029\000\
\\014\000\022\000\028\000\010\000\
\\036\000\036\000\015\000\003\000\
\\006\000\023\000\037\000\029\000\
\\019\000\004\000\005\000\011\000\
\\002\000\020\000\030\000\031\000\
\\024\000\025\000\036\000\007\000\
\\008\000\035\000\039\000\038\000\
\\016\000\026\000\027\000\032\000\
\\012\000\013\000\036\000\036\000\
\\017\000\018\000\034\000\033\000\
\\000\000"
val gotoT =
"\
\\001\000\044\000\000\000\
\\000\000\
\\000\000\
\\002\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\010\000\004\000\009\000\000\000\
\\003\000\014\000\004\000\009\000\000\000\
\\000\000\
\\005\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\032\000\004\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\040\000\004\000\009\000\000\000\
\\003\000\041\000\004\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 45
val numrules = 12
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | BOOL of unit ->  (bool) | ID of unit ->  (string)
 | INT of unit ->  (int) | path of unit ->  (Path.t)
 | stmt of unit ->  (SStmt.t) | stmts of unit ->  (SStmt.t list)
 | cases of unit ->  (ScheduleCase.t list)
 | schedule of unit ->  (Schedule.t)
end
type svalue = MlyValue.svalue
type result = Schedule.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 1) => true | (T 2) => true | (T 3) => true | (T 4) => true | (T 
5) => true | (T 6) => true | (T 9) => true | (T 10) => true | (T 11)
 => true | (T 12) => true | (T 13) => true | (T 14) => true | (T 15)
 => true | (T 16) => true | (T 17) => true | (T 18) => true | (T 21)
 => true | (T 22) => true | (T 23) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 22))::
(nil
,nil
 $$ (T 23))::
(nil
,nil
 $$ (T 37))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "TRAVERSAL"
  | (T 2) => "CASE"
  | (T 3) => "RECUR"
  | (T 4) => "ITERATE"
  | (T 5) => "LEFT"
  | (T 6) => "RIGHT"
  | (T 7) => "EVAL"
  | (T 8) => "HOLE"
  | (T 9) => "INTERFACE"
  | (T 10) => "INPUT"
  | (T 11) => "OUTPUT"
  | (T 12) => "TINT"
  | (T 13) => "TBOOL"
  | (T 14) => "TSET"
  | (T 15) => "CLASS"
  | (T 16) => "CHILDREN"
  | (T 17) => "RULES"
  | (T 18) => "SELF"
  | (T 19) => "ASSIGN"
  | (T 20) => "DOT"
  | (T 21) => "IF"
  | (T 22) => "THEN"
  | (T 23) => "ELSE"
  | (T 24) => "OR"
  | (T 25) => "AND"
  | (T 26) => "NOT"
  | (T 27) => "EQ"
  | (T 28) => "NEQ"
  | (T 29) => "LT"
  | (T 30) => "LE"
  | (T 31) => "GT"
  | (T 32) => "GE"
  | (T 33) => "PLUS"
  | (T 34) => "MINUS"
  | (T 35) => "TIMES"
  | (T 36) => "DIVIDE"
  | (T 37) => "LPAREN"
  | (T 38) => "RPAREN"
  | (T 39) => "LBRACK"
  | (T 40) => "RBRACK"
  | (T 41) => "LBRACE"
  | (T 42) => "RBRACE"
  | (T 43) => "COLON"
  | (T 44) => "SEMICOLON"
  | (T 45) => "COMMA"
  | (T 46) => "INT"
  | (T 47) => "ID"
  | (T 48) => "BOOL"
  | (T 49) => "PREC_UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 47) => MlyValue.ID(fn () => ("bogus")) | 
(T 48) => MlyValue.BOOL(fn () => (true)) | 
(T 46) => MlyValue.INT(fn () => (1)) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 49) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40)
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.cases cases1
, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
TRAVERSAL1left, _)) :: rest671)) => let val  result = 
MlyValue.schedule (fn _ => let val  (ID as ID1) = ID1 ()
 val  (cases as cases1) = cases1 ()
 in ((ID, cases))
end)
 in ( LrTable.NT 0, ( result, TRAVERSAL1left, RBRACE1right), rest671)

end
|  ( 1, ( rest671)) => let val  result = MlyValue.cases (fn _ => ([]))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.cases cases1, _, cases1right)) :: _ :: ( _, 
( MlyValue.stmts stmts1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, CASE1left, _)) :: rest671)) => let val  result = 
MlyValue.cases (fn _ => let val  (ID as ID1) = ID1 ()
 val  (stmts as stmts1) = stmts1 ()
 val  (cases as cases1) = cases1 ()
 in ((ID, stmts) :: cases)
end)
 in ( LrTable.NT 1, ( result, CASE1left, cases1right), rest671)
end
|  ( 3, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.path path1
, _, _)) :: ( _, ( _, EVAL1left, _)) :: rest671)) => let val  result =
 MlyValue.stmt (fn _ => let val  (path as path1) = path1 ()
 in (SStmt.Eval(path))
end)
 in ( LrTable.NT 3, ( result, EVAL1left, SEMICOLON1right), rest671)

end
|  ( 4, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.stmts stmts1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
ITERATE1left, _)) :: rest671)) => let val  result = MlyValue.stmt (fn
 _ => let val  (ID as ID1) = ID1 ()
 val  (stmts as stmts1) = stmts1 ()
 in (SStmt.Iterate(NONE, ID, stmts))
end)
 in ( LrTable.NT 3, ( result, ITERATE1left, RBRACE1right), rest671)

end
|  ( 5, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.stmts stmts1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: _ :: _ :: ( _, 
( _, ITERATE1left, _)) :: rest671)) => let val  result = MlyValue.stmt
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (stmts as stmts1) = stmts1 ()
 in (SStmt.Iterate(SOME(SStmt.Left), ID, stmts))
end)
 in ( LrTable.NT 3, ( result, ITERATE1left, RBRACE1right), rest671)

end
|  ( 6, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.stmts stmts1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: _ :: _ :: ( _, 
( _, ITERATE1left, _)) :: rest671)) => let val  result = MlyValue.stmt
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (stmts as stmts1) = stmts1 ()
 in (SStmt.Iterate(SOME(SStmt.Right), ID, stmts))
end)
 in ( LrTable.NT 3, ( result, ITERATE1left, RBRACE1right), rest671)

end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.ID ID1, _,
 _)) :: ( _, ( _, RECUR1left, _)) :: rest671)) => let val  result = 
MlyValue.stmt (fn _ => let val  (ID as ID1) = ID1 ()
 in (SStmt.Recur(ID))
end)
 in ( LrTable.NT 3, ( result, RECUR1left, SEMICOLON1right), rest671)

end
|  ( 8, ( rest671)) => let val  result = MlyValue.stmts (fn _ => ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( MlyValue.stmts stmts1, _, stmts1right)) :: ( _, ( 
MlyValue.stmt stmt1, stmt1left, _)) :: rest671)) => let val  result = 
MlyValue.stmts (fn _ => let val  (stmt as stmt1) = stmt1 ()
 val  (stmts as stmts1) = stmts1 ()
 in (stmt :: stmts)
end)
 in ( LrTable.NT 2, ( result, stmt1left, stmts1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
SELF1left, _)) :: rest671)) => let val  result = MlyValue.path (fn _
 => let val  (ID as ID1) = ID1 ()
 in (Path.Dot(Path.Self, ID))
end)
 in ( LrTable.NT 4, ( result, SELF1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.path (fn _ => let val  (ID as ID1) = ID1 ()
 val  ID2 = ID2 ()
 in (Path.Dot(Path.Child(ID), ID2))
end)
 in ( LrTable.NT 4, ( result, ID1left, ID2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.schedule x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : HecateSchedule_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TRAVERSAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RECUR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ITERATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LEFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RIGHT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EVAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun HOLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun INTERFACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun INPUT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun OUTPUT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TBOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TSET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun CLASS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun CHILDREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RULES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun SELF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun PREC_UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
end
end
