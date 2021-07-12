structure Prelude = struct

fun intercalate sep [] = ""
  | intercalate sep [s] = s
  | intercalate sep (s::ss) = s ^ sep ^ intercalate sep ss

val unwords = intercalate " "
val join = intercalate "\n"

fun id x = x

end