signature Hecate_TOKENS =
sig
type ('a,'b) token
type svalue
val PREC_UMINUS:  'a * 'a -> (svalue,'a) token
val NULL:  'a * 'a -> (svalue,'a) token
val BOOL: (bool) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val GE:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LE:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val SELF:  'a * 'a -> (svalue,'a) token
val RULES:  'a * 'a -> (svalue,'a) token
val CHILDREN:  'a * 'a -> (svalue,'a) token
val CLASS:  'a * 'a -> (svalue,'a) token
val TSET:  'a * 'a -> (svalue,'a) token
val TBOOL:  'a * 'a -> (svalue,'a) token
val TINT:  'a * 'a -> (svalue,'a) token
val OUTPUT:  'a * 'a -> (svalue,'a) token
val INPUT:  'a * 'a -> (svalue,'a) token
val INTERFACE:  'a * 'a -> (svalue,'a) token
val HOLE:  'a * 'a -> (svalue,'a) token
val EVAL:  'a * 'a -> (svalue,'a) token
val RIGHT:  'a * 'a -> (svalue,'a) token
val LEFT:  'a * 'a -> (svalue,'a) token
val ITERATE:  'a * 'a -> (svalue,'a) token
val RECUR:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val TRAVERSAL:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Hecate_LRVALS=
sig
structure Tokens : Hecate_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
