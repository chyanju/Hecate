structure Prelude = struct

fun intercalate sep [] = ""
  | intercalate sep [s] = s
  | intercalate sep (s::ss) = s ^ sep ^ intercalate sep ss

val unwords = intercalate " "
val join = intercalate "\n"

fun id x = x

fun optToString NONE = ""
  | optToString (SOME s) = s

(* fun mergeTables (t1: ('a,'b) hash_table) (t2: ('a,'c) hash_table) : ('a, 'b * 'c) hash_table = *)

end