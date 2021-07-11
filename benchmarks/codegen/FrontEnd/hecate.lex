structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun eof pos = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun inc r = (r := !r + 1)

%% 
%header (functor HecateLexFun(structure Tokens: Hecate_TOKENS));
ws = [\ \t];
digit = [0-9];
letter=[A-Za-z];
%s COMMENT;
%%
<INITIAL>\n	=> (inc lineNum; linePos := yypos :: !linePos; continue());
<INITIAL>{ws}+  => (continue());


<INITIAL>"traversal"    => (Tokens.TRAVERSAL(yypos, yypos + size yytext));
<INITIAL>"case"      => (Tokens.CASE(yypos, yypos + size yytext));
<INITIAL>"recur"       => (Tokens.RECUR(yypos, yypos + size yytext));
<INITIAL>"iterate"    => (Tokens.ITERATE(yypos, yypos + size yytext));
<INITIAL>"left"       => (Tokens.LEFT(yypos, yypos + size yytext));
<INITIAL>"right"       => (Tokens.RIGHT(yypos, yypos + size yytext));
<INITIAL>"eval"       => (Tokens.EVAL(yypos, yypos + size yytext));

<INITIAL>"interface"      => (Tokens.INTERFACE(yypos, yypos + size yytext));
<INITIAL>"input"       => (Tokens.INPUT(yypos, yypos + size yytext));
<INITIAL>"output"      => (Tokens.OUTPUT(yypos, yypos + size yytext));
<INITIAL>"int"       => (Tokens.TINT(yypos, yypos + size yytext));
<INITIAL>"bool"      => (Tokens.TBOOL(yypos, yypos + size yytext));
<INITIAL>"set"       => (Tokens.TSET(yypos, yypos + size yytext));
<INITIAL>"class"     => (Tokens.CLASS(yypos, yypos + size yytext));
<INITIAL>"children"    => (Tokens.CHILDREN(yypos, yypos + size yytext));
<INITIAL>"rules"       => (Tokens.RULES(yypos, yypos + size yytext));
<INITIAL>"self"        => (Tokens.SELF(yypos, yypos + size yytext));

<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos + size yytext));
<INITIAL>"." => (Tokens.DOT(yypos, yypos + size yytext));
<INITIAL>"if"       => (Tokens.IF(yypos, yypos + size yytext));
<INITIAL>"then"     => (Tokens.THEN(yypos, yypos + size yytext));
<INITIAL>"else"     => (Tokens.ELSE(yypos, yypos + size yytext));

<INITIAL>"&&" => (Tokens.AND(yypos, yypos + size yytext));
<INITIAL>"||" => (Tokens.OR(yypos, yypos + size yytext));
<INITIAL>"!" => (Tokens.NOT(yypos, yypos + size yytext));

<INITIAL>"==" => (Tokens.EQ(yypos, yypos + size yytext));
<INITIAL>"!=" => (Tokens.NEQ(yypos, yypos + size yytext));
<INITIAL>"<" => (Tokens.LT(yypos, yypos + size yytext));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos + size yytext));
<INITIAL>">" => (Tokens.GT(yypos, yypos + size yytext));
<INITIAL>">=" => (Tokens.GE(yypos, yypos + size yytext));

<INITIAL>"+" => (Tokens.PLUS(yypos, yypos + size yytext));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos + size yytext));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos + size yytext));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos + size yytext));

<INITIAL>"??" => (Tokens.HOLE(yypos, yypos + size yytext));

<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos + size yytext));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos + size yytext));

<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos + size yytext));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos + size yytext));

<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos + size yytext));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos + size yytext));

<INITIAL>":" => (Tokens.COLON(yypos, yypos + size yytext));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos + size yytext));
<INITIAL>"," => (Tokens.COMMA(yypos, yypos + size yytext));

<INITIAL>"true" => (Tokens.BOOL(true, yypos, yypos + size yytext));
<INITIAL>"false" => (Tokens.BOOL(false, yypos, yypos + size yytext));
<INITIAL>"null" => (Tokens.NULL(yypos, yypos + size yytext));
<INITIAL>{digit}+  => (case Int.fromString yytext of
                NONE   => (ErrorMsg.error yypos ("illegal integer " ^ yytext); continue())
              | SOME n => Tokens.INT(n, yypos, yypos + size yytext));
<INITIAL>{letter}({letter}|{digit}|"_")* => (Tokens.ID(yytext, yypos, yypos + size yytext));


<INITIAL>"//" => (YYBEGIN COMMENT; continue());
<COMMENT>"\n" => (inc lineNum; YYBEGIN INITIAL; continue());
<COMMENT>.    => (continue());

<INITIAL>.          => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());