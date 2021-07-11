functor HecateLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Hecate_TOKENS
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
\\001\000\002\000\004\000\000\000\
\\001\000\013\000\042\000\014\000\041\000\015\000\040\000\000\000\
\\001\000\017\000\038\000\000\000\
\\001\000\018\000\060\000\000\000\
\\001\000\019\000\070\000\022\000\087\000\027\000\086\000\035\000\085\000\
\\038\000\084\000\039\000\124\000\040\000\083\000\047\000\082\000\
\\048\000\081\000\049\000\080\000\000\000\
\\001\000\019\000\070\000\022\000\087\000\027\000\086\000\035\000\085\000\
\\038\000\084\000\040\000\083\000\047\000\082\000\048\000\081\000\
\\049\000\080\000\000\000\
\\001\000\019\000\070\000\048\000\069\000\000\000\
\\001\000\020\000\074\000\000\000\
\\001\000\021\000\075\000\000\000\
\\001\000\021\000\075\000\038\000\103\000\000\000\
\\001\000\021\000\076\000\000\000\
\\001\000\023\000\182\000\024\000\182\000\025\000\182\000\026\000\182\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\039\000\182\000\043\000\182\000\045\000\182\000\046\000\182\000\000\000\
\\001\000\023\000\183\000\024\000\183\000\025\000\183\000\026\000\183\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\039\000\183\000\043\000\183\000\045\000\183\000\046\000\183\000\000\000\
\\001\000\023\000\184\000\024\000\184\000\025\000\184\000\026\000\184\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\039\000\184\000\043\000\184\000\045\000\184\000\046\000\184\000\000\000\
\\001\000\023\000\185\000\024\000\185\000\025\000\185\000\026\000\185\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\039\000\185\000\043\000\185\000\045\000\185\000\046\000\185\000\000\000\
\\001\000\023\000\186\000\024\000\186\000\025\000\186\000\026\000\186\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\039\000\186\000\043\000\186\000\045\000\186\000\046\000\186\000\000\000\
\\001\000\023\000\187\000\024\000\187\000\025\000\187\000\026\000\187\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\039\000\187\000\043\000\187\000\045\000\187\000\046\000\187\000\000\000\
\\001\000\023\000\127\000\025\000\102\000\026\000\101\000\028\000\100\000\
\\029\000\099\000\030\000\098\000\031\000\097\000\032\000\096\000\
\\033\000\095\000\034\000\094\000\035\000\093\000\036\000\092\000\
\\037\000\091\000\000\000\
\\001\000\024\000\134\000\025\000\102\000\026\000\101\000\028\000\100\000\
\\029\000\099\000\030\000\098\000\031\000\097\000\032\000\096\000\
\\033\000\095\000\034\000\094\000\035\000\093\000\036\000\092\000\
\\037\000\091\000\000\000\
\\001\000\025\000\102\000\026\000\101\000\028\000\100\000\029\000\099\000\
\\030\000\098\000\031\000\097\000\032\000\096\000\033\000\095\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\039\000\126\000\000\000\
\\001\000\025\000\102\000\026\000\101\000\028\000\100\000\029\000\099\000\
\\030\000\098\000\031\000\097\000\032\000\096\000\033\000\095\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\043\000\138\000\000\000\
\\001\000\025\000\102\000\026\000\101\000\028\000\100\000\029\000\099\000\
\\030\000\098\000\031\000\097\000\032\000\096\000\033\000\095\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\045\000\090\000\000\000\
\\001\000\039\000\128\000\000\000\
\\001\000\040\000\063\000\048\000\062\000\000\000\
\\001\000\041\000\072\000\000\000\
\\001\000\041\000\125\000\000\000\
\\001\000\042\000\013\000\000\000\
\\001\000\042\000\017\000\000\000\
\\001\000\042\000\030\000\000\000\
\\001\000\042\000\031\000\000\000\
\\001\000\042\000\048\000\000\000\
\\001\000\042\000\051\000\000\000\
\\001\000\042\000\064\000\000\000\
\\001\000\042\000\130\000\000\000\
\\001\000\043\000\025\000\000\000\
\\001\000\043\000\029\000\000\000\
\\001\000\043\000\044\000\000\000\
\\001\000\043\000\057\000\000\000\
\\001\000\043\000\059\000\000\000\
\\001\000\043\000\073\000\000\000\
\\001\000\043\000\077\000\000\000\
\\001\000\044\000\020\000\000\000\
\\001\000\044\000\032\000\000\000\
\\001\000\044\000\058\000\000\000\
\\001\000\044\000\135\000\000\000\
\\001\000\045\000\045\000\000\000\
\\001\000\045\000\049\000\000\000\
\\001\000\045\000\052\000\000\000\
\\001\000\045\000\065\000\000\000\
\\001\000\047\000\104\000\000\000\
\\001\000\048\000\008\000\000\000\
\\001\000\048\000\012\000\000\000\
\\001\000\048\000\016\000\000\000\
\\001\000\048\000\026\000\000\000\
\\001\000\048\000\027\000\000\000\
\\001\000\048\000\028\000\000\000\
\\001\000\048\000\046\000\000\000\
\\001\000\048\000\047\000\000\000\
\\001\000\048\000\066\000\000\000\
\\001\000\048\000\088\000\000\000\
\\001\000\048\000\089\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\003\000\019\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\004\000\037\000\005\000\036\000\009\000\035\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\011\000\024\000\012\000\023\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\010\000\007\000\000\000\
\\158\000\000\000\
\\159\000\016\000\011\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\048\000\054\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\019\000\070\000\048\000\069\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\025\000\102\000\026\000\101\000\028\000\100\000\029\000\099\000\
\\030\000\098\000\031\000\097\000\032\000\096\000\033\000\095\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\000\000\
\\175\000\000\000\
\\176\000\036\000\092\000\037\000\091\000\000\000\
\\177\000\036\000\092\000\037\000\091\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\028\000\100\000\029\000\099\000\030\000\098\000\031\000\097\000\
\\032\000\096\000\033\000\095\000\034\000\094\000\035\000\093\000\
\\036\000\092\000\037\000\091\000\000\000\
\\181\000\026\000\101\000\028\000\100\000\029\000\099\000\030\000\098\000\
\\031\000\097\000\032\000\096\000\033\000\095\000\034\000\094\000\
\\035\000\093\000\036\000\092\000\037\000\091\000\000\000\
\\188\000\025\000\102\000\026\000\101\000\028\000\100\000\029\000\099\000\
\\030\000\098\000\031\000\097\000\032\000\096\000\033\000\095\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\025\000\102\000\026\000\101\000\028\000\100\000\029\000\099\000\
\\030\000\098\000\031\000\097\000\032\000\096\000\033\000\095\000\
\\034\000\094\000\035\000\093\000\036\000\092\000\037\000\091\000\
\\046\000\129\000\000\000\
\\193\000\000\000\
\"
val actionRowNumbers =
"\001\000\079\000\051\000\062\000\
\\081\000\052\000\027\000\081\000\
\\079\000\053\000\028\000\064\000\
\\082\000\080\000\042\000\072\000\
\\035\000\054\000\055\000\056\000\
\\036\000\075\000\074\000\063\000\
\\029\000\030\000\043\000\071\000\
\\069\000\003\000\002\000\069\000\
\\037\000\046\000\057\000\058\000\
\\031\000\047\000\078\000\077\000\
\\076\000\070\000\064\000\066\000\
\\032\000\048\000\084\000\072\000\
\\065\000\069\000\068\000\038\000\
\\044\000\073\000\039\000\004\000\
\\024\000\067\000\033\000\049\000\
\\086\000\059\000\088\000\084\000\
\\025\000\040\000\008\000\009\000\
\\011\000\085\000\087\000\041\000\
\\006\000\060\000\061\000\083\000\
\\095\000\022\000\094\000\010\000\
\\093\000\050\000\006\000\006\000\
\\006\000\006\000\091\000\090\000\
\\088\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\005\000\026\000\020\000\
\\097\000\096\000\018\000\089\000\
\\101\000\100\000\099\000\098\000\
\\015\000\014\000\017\000\016\000\
\\013\000\012\000\102\000\103\000\
\\023\000\108\000\106\000\034\000\
\\092\000\006\000\107\000\006\000\
\\007\000\019\000\109\000\045\000\
\\006\000\006\000\104\000\021\000\
\\105\000\000\000"
val gotoT =
"\
\\001\000\137\000\002\000\001\000\000\000\
\\006\000\004\000\010\000\003\000\000\000\
\\000\000\
\\000\000\
\\011\000\008\000\012\000\007\000\000\000\
\\000\000\
\\000\000\
\\011\000\012\000\012\000\007\000\000\000\
\\006\000\004\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\003\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\020\000\008\000\019\000\000\000\
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
\\000\000\
\\004\000\032\000\005\000\031\000\000\000\
\\000\000\
\\009\000\037\000\000\000\
\\004\000\041\000\005\000\031\000\000\000\
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
\\003\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\051\000\000\000\
\\007\000\053\000\008\000\019\000\000\000\
\\000\000\
\\004\000\054\000\005\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\066\000\015\000\065\000\000\000\
\\016\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\077\000\014\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\103\000\014\000\076\000\000\000\
\\013\000\104\000\014\000\076\000\000\000\
\\013\000\105\000\014\000\076\000\000\000\
\\013\000\106\000\014\000\076\000\000\000\
\\000\000\
\\000\000\
\\014\000\066\000\015\000\107\000\000\000\
\\013\000\108\000\014\000\076\000\000\000\
\\013\000\109\000\014\000\076\000\000\000\
\\013\000\110\000\014\000\076\000\000\000\
\\013\000\111\000\014\000\076\000\000\000\
\\013\000\112\000\014\000\076\000\000\000\
\\013\000\113\000\014\000\076\000\000\000\
\\013\000\114\000\014\000\076\000\000\000\
\\013\000\115\000\014\000\076\000\000\000\
\\013\000\116\000\014\000\076\000\000\000\
\\013\000\117\000\014\000\076\000\000\000\
\\013\000\118\000\014\000\076\000\000\000\
\\013\000\119\000\014\000\076\000\000\000\
\\013\000\121\000\014\000\076\000\018\000\120\000\000\000\
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
\\000\000\
\\013\000\129\000\014\000\076\000\000\000\
\\000\000\
\\013\000\121\000\014\000\076\000\018\000\130\000\000\000\
\\014\000\131\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\134\000\014\000\076\000\000\000\
\\013\000\135\000\014\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 138
val numrules = 54
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
 | INT of unit ->  (int) | args of unit ->  (Expr.t list)
 | ctyp of unit ->  (CType.t) | children of unit ->  (Child.t list)
 | rules of unit ->  (Rule.t list) | path of unit ->  (Path.t)
 | expr of unit ->  (Expr.t) | class of unit ->  (Class.t)
 | classes of unit ->  (Class.t list)
 | groups of unit ->  (Group.t list) | typ of unit ->  (Type.t)
 | io of unit ->  (InOut.t) | fields of unit ->  (Field.t list)
 | interface of unit ->  (Interface.t) | tstmt of unit ->  (TStmt.t)
 | tstmts of unit ->  (TStmt.t list) | cases of unit ->  (Case.t list)
 | traversal of unit ->  (Traversal.t)
 | program of unit ->  (Group.t list)
end
type svalue = MlyValue.svalue
type result = Group.t list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 1) => true | (T 2) => true | (T 3) => true | (T 4) => true | (T 
5) => true | (T 6) => true | (T 7) => true | (T 9) => true | (T 10)
 => true | (T 11) => true | (T 12) => true | (T 13) => true | (T 14)
 => true | (T 15) => true | (T 16) => true | (T 17) => true | (T 18)
 => true | (T 21) => true | (T 22) => true | (T 23) => true | _ => false
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
of  ( 0, ( ( _, ( MlyValue.groups groups1, _, groups1right)) :: ( _, (
 MlyValue.traversal traversal1, traversal1left, _)) :: rest671)) =>
 let val  result = MlyValue.program (fn _ => let val  traversal1 = 
traversal1 ()
 val  (groups as groups1) = groups1 ()
 in (groups)
end)
 in ( LrTable.NT 0, ( result, traversal1left, groups1right), rest671)

end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.cases cases1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
TRAVERSAL1left, _)) :: rest671)) => let val  result = 
MlyValue.traversal (fn _ => let val  (ID as ID1) = ID1 ()
 val  (cases as cases1) = cases1 ()
 in ((ID, cases))
end)
 in ( LrTable.NT 1, ( result, TRAVERSAL1left, RBRACE1right), rest671)

end
|  ( 2, ( rest671)) => let val  result = MlyValue.cases (fn _ => ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.cases cases1, _, cases1right)) :: _ :: ( _, 
( MlyValue.tstmts tstmts1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _
)) :: ( _, ( _, CASE1left, _)) :: rest671)) => let val  result = 
MlyValue.cases (fn _ => let val  (ID as ID1) = ID1 ()
 val  (tstmts as tstmts1) = tstmts1 ()
 val  (cases as cases1) = cases1 ()
 in ((ID,tstmts) :: cases)
end)
 in ( LrTable.NT 2, ( result, CASE1left, cases1right), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( _, HOLE1left, _))
 :: rest671)) => let val  result = MlyValue.tstmt (fn _ => (
TStmt.SHole))
 in ( LrTable.NT 4, ( result, HOLE1left, SEMICOLON1right), rest671)

end
|  ( 5, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tstmts 
tstmts1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
ITERATE1left, _)) :: rest671)) => let val  result = MlyValue.tstmt (fn
 _ => let val  (ID as ID1) = ID1 ()
 val  (tstmts as tstmts1) = tstmts1 ()
 in (TStmt.SIterate(ID, tstmts))
end)
 in ( LrTable.NT 4, ( result, ITERATE1left, RBRACE1right), rest671)

end
|  ( 6, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.ID ID1, _,
 _)) :: ( _, ( _, RECUR1left, _)) :: rest671)) => let val  result = 
MlyValue.tstmt (fn _ => let val  (ID as ID1) = ID1 ()
 in (TStmt.SRecur(ID))
end)
 in ( LrTable.NT 4, ( result, RECUR1left, SEMICOLON1right), rest671)

end
|  ( 7, ( rest671)) => let val  result = MlyValue.tstmts (fn _ => ([])
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( MlyValue.tstmts tstmts1, _, tstmts1right)) :: ( _, ( 
MlyValue.tstmt tstmt1, tstmt1left, _)) :: rest671)) => let val  result
 = MlyValue.tstmts (fn _ => let val  (tstmt as tstmt1) = tstmt1 ()
 val  (tstmts as tstmts1) = tstmts1 ()
 in (tstmt :: tstmts)
end)
 in ( LrTable.NT 3, ( result, tstmt1left, tstmts1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.fields 
fields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
INTERFACE1left, _)) :: rest671)) => let val  result = 
MlyValue.interface (fn _ => let val  (ID as ID1) = ID1 ()
 val  (fields as fields1) = fields1 ()
 in ((ID, fields))
end)
 in ( LrTable.NT 5, ( result, INTERFACE1left, RBRACE1right), rest671)

end
|  ( 10, ( rest671)) => let val  result = MlyValue.fields (fn _ => ([]
))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.fields fields1, _, fields1right)) :: _ :: (
 _, ( MlyValue.typ typ1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( MlyValue.io io1, io1left, _)) :: rest671)) => let val  
result = MlyValue.fields (fn _ => let val  (io as io1) = io1 ()
 val  (ID as ID1) = ID1 ()
 val  (typ as typ1) = typ1 ()
 val  (fields as fields1) = fields1 ()
 in ((io, ID, typ) :: fields)
end)
 in ( LrTable.NT 6, ( result, io1left, fields1right), rest671)
end
|  ( 12, ( ( _, ( _, INPUT1left, INPUT1right)) :: rest671)) => let
 val  result = MlyValue.io (fn _ => (InOut.I))
 in ( LrTable.NT 7, ( result, INPUT1left, INPUT1right), rest671)
end
|  ( 13, ( ( _, ( _, OUTPUT1left, OUTPUT1right)) :: rest671)) => let
 val  result = MlyValue.io (fn _ => (InOut.O))
 in ( LrTable.NT 7, ( result, OUTPUT1left, OUTPUT1right), rest671)
end
|  ( 14, ( ( _, ( _, TINT1left, TINT1right)) :: rest671)) => let val  
result = MlyValue.typ (fn _ => (Type.Int))
 in ( LrTable.NT 8, ( result, TINT1left, TINT1right), rest671)
end
|  ( 15, ( ( _, ( _, TBOOL1left, TBOOL1right)) :: rest671)) => let
 val  result = MlyValue.typ (fn _ => (Type.Bool))
 in ( LrTable.NT 8, ( result, TBOOL1left, TBOOL1right), rest671)
end
|  ( 16, ( ( _, ( _, TSET1left, TSET1right)) :: rest671)) => let val  
result = MlyValue.typ (fn _ => (Type.Set))
 in ( LrTable.NT 8, ( result, TSET1left, TSET1right), rest671)
end
|  ( 17, ( rest671)) => let val  result = MlyValue.groups (fn _ => ([]
))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 18, ( ( _, ( MlyValue.groups groups1, _, groups1right)) :: ( _, (
 MlyValue.classes classes1, _, _)) :: ( _, ( MlyValue.interface 
interface1, interface1left, _)) :: rest671)) => let val  result = 
MlyValue.groups (fn _ => let val  (interface as interface1) = 
interface1 ()
 val  (classes as classes1) = classes1 ()
 val  (groups as groups1) = groups1 ()
 in (Group.T {interface=interface, classes=classes} :: groups)
end)
 in ( LrTable.NT 9, ( result, interface1left, groups1right), rest671)

end
|  ( 19, ( rest671)) => let val  result = MlyValue.classes (fn _ => (
[]))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 20, ( ( _, ( MlyValue.classes classes1, _, classes1right)) :: ( _
, ( MlyValue.class class1, class1left, _)) :: rest671)) => let val  
result = MlyValue.classes (fn _ => let val  (class as class1) = class1
 ()
 val  (classes as classes1) = classes1 ()
 in (class :: classes)
end)
 in ( LrTable.NT 10, ( result, class1left, classes1right), rest671)

end
|  ( 21, ( ( _, ( _, _, RBRACE3right)) :: _ :: ( _, ( MlyValue.rules 
rules1, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.children children1, _
, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, CLASS1left, _)) :: rest671)) =>
 let val  result = MlyValue.class (fn _ => let val  (ID as ID1) = ID1
 ()
 val  ID2 = ID2 ()
 val  (children as children1) = children1 ()
 val  (rules as rules1) = rules1 ()
 in (Class.T{name=ID, interface=ID2, children=children, rules=rules})

end)
 in ( LrTable.NT 11, ( result, CLASS1left, RBRACE3right), rest671)
end
|  ( 22, ( rest671)) => let val  result = MlyValue.children (fn _ => (
[]))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 23, ( ( _, ( MlyValue.children children1, _, children1right)) ::
 _ :: ( _, ( MlyValue.ctyp ctyp1, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.children
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ctyp as ctyp1) = ctyp1 ()
 val  (children as children1) = children1 ()
 in ((ID,ctyp) :: children)
end)
 in ( LrTable.NT 15, ( result, ID1left, children1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ctyp (fn _ => let val  (ID as ID1) = ID1
 ()
 in (CType.Interface(ID))
end)
 in ( LrTable.NT 16, ( result, ID1left, ID1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ID ID1, _, _
)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let val  result = 
MlyValue.ctyp (fn _ => let val  (ID as ID1) = ID1 ()
 in (CType.List(ID))
end)
 in ( LrTable.NT 16, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 26, ( rest671)) => let val  result = MlyValue.rules (fn _ => ([])
)
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 27, ( ( _, ( MlyValue.rules rules1, _, rules1right)) :: _ :: ( _,
 ( MlyValue.expr expr1, _, _)) :: _ :: ( _, ( MlyValue.path path1, 
path1left, _)) :: rest671)) => let val  result = MlyValue.rules (fn _
 => let val  (path as path1) = path1 ()
 val  (expr as expr1) = expr1 ()
 val  (rules as rules1) = rules1 ()
 in ((path, expr) :: rules)
end)
 in ( LrTable.NT 14, ( result, path1left, rules1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
SELF1left, _)) :: rest671)) => let val  result = MlyValue.path (fn _
 => let val  (ID as ID1) = ID1 ()
 in (Path.Dot(Path.Self, ID))
end)
 in ( LrTable.NT 13, ( result, SELF1left, ID1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.path (fn _ => let val  (ID as ID1) = ID1 ()
 val  ID2 = ID2 ()
 in (Path.Dot(Path.Child(ID), ID2))
end)
 in ( LrTable.NT 13, ( result, ID1left, ID2right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (expr)
end)
 in ( LrTable.NT 12, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.expr (fn _ => let val  (INT as INT1) =
 INT1 ()
 in (Expr.Int(INT))
end)
 in ( LrTable.NT 12, ( result, INT1left, INT1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in (Expr.Bool(BOOL))
end)
 in ( LrTable.NT 12, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.path path1, path1left, path1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (path
 as path1) = path1 ()
 in (Expr.Path path)
end)
 in ( LrTable.NT 12, ( result, path1left, path1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn _ =>
 let val  (expr as expr1) = expr1 ()
 in (Expr.Unop(Unop.Not, expr))
end)
 in ( LrTable.NT 12, ( result, NOT1left, expr1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (expr as expr1) = expr1 ()
 in (Expr.Binop(Binop.Minus, Expr.Int(0), expr))
end)
 in ( LrTable.NT 12, ( result, MINUS1left, expr1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binop(Binop.Plus, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binop(Binop.Minus, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binop(Binop.Times, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binop(Binop.Divide, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binop(Binop.And, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binop(Binop.Or, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binrel(Binrel.EQ, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binrel(Binrel.NEQ, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binrel(Binrel.GT, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binrel(Binrel.GE, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binrel(Binrel.LT, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Expr.Binrel(Binrel.LE, expr1, expr2))
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.expr expr3, _, expr3right)) :: _ :: ( _, ( 
MlyValue.expr expr2, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 val  expr3 = expr3 ()
 in (Expr.If(expr1, expr2, expr3))
end)
 in ( LrTable.NT 12, ( result, IF1left, expr3right), rest671)
end
|  ( 49, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( MlyValue.path path1, _, _)) :: _ :: _ :: ( _, (
 MlyValue.INT INT1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  (INT as INT1) = 
INT1 ()
 val  (path as path1) = path1 ()
 val  (expr as expr1) = expr1 ()
 in (Expr.Hack(INT, path, expr))
end)
 in ( LrTable.NT 12, ( result, LBRACK1left, RBRACE1right), rest671)

end
|  ( 50, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 ID1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (Expr.Call(ID, []))
end)
 in ( LrTable.NT 12, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 51, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.args args1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) =>
 let val  result = MlyValue.expr (fn _ => let val  (ID as ID1) = ID1
 ()
 val  (args as args1) = args1 ()
 in (Expr.Call(ID, args))
end)
 in ( LrTable.NT 12, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.expr expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.args (fn _ => let val  (expr
 as expr1) = expr1 ()
 in ([expr])
end)
 in ( LrTable.NT 17, ( result, expr1left, expr1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.args args1, _, args1right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.args (fn _ => let val  (expr as expr1) = expr1 ()
 val  (args as args1) = args1 ()
 in (expr :: args)
end)
 in ( LrTable.NT 17, ( result, expr1left, args1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Hecate_TOKENS =
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
