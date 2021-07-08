signature TOKENS =
sig
type linenum (* = int *)
type token

(* traversal language *)
val TRAVERSAL:  linenum * linenum -> token
val CASE:  linenum * linenum -> token
val RECUR:  linenum * linenum -> token
val ITERATE:  linenum * linenum -> token
val HOLE:  linenum * linenum -> token

(* attribute language *)
val INTERFACE:  linenum * linenum -> token
val INPUT:  linenum * linenum -> token
val OUTPUT:  linenum * linenum -> token
val TINT:  linenum * linenum -> token
val TBOOL:  linenum * linenum -> token
val CLASS:  linenum * linenum -> token
val CHILDREN:  linenum * linenum -> token
val RULES:  linenum * linenum -> token
val SELF:   linenum * linenum -> token

(* expressions and others*)
val ASSIGN:  linenum * linenum -> token
val DOT:  linenum * linenum -> token
val IF:  linenum * linenum -> token
val THEN:  linenum * linenum -> token
val ELSE:  linenum * linenum -> token

val OR:  linenum * linenum -> token
val AND:  linenum * linenum -> token
val NOT:  linenum * linenum -> token

val GE:  linenum * linenum -> token
val GT:  linenum * linenum -> token
val LE:  linenum * linenum -> token
val LT:  linenum * linenum -> token
val NEQ:  linenum * linenum -> token
val EQ:  linenum * linenum -> token

val PLUS:  linenum * linenum -> token
val MINUS:  linenum * linenum -> token
val TIMES:  linenum * linenum -> token
val DIVIDE:  linenum * linenum -> token

val RBRACE:  linenum * linenum -> token
val LBRACE:  linenum * linenum -> token
val RBRACK:  linenum * linenum -> token
val LBRACK:  linenum * linenum -> token
val RPAREN:  linenum * linenum -> token
val LPAREN:  linenum * linenum -> token
val SEMICOLON:  linenum * linenum -> token
val COLON:  linenum * linenum -> token

val BOOL: (bool) * linenum * linenum -> token
val INT: (int) * linenum * linenum -> token
val ID: (string) * linenum * linenum -> token

val EOF:  linenum * linenum -> token
end