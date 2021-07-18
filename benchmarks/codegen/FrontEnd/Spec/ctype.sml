structure CType = struct
  datatype t =
      Interface of string
    | List of string
    | Option of string
  
  fun toString (Interface itf) = itf
    | toString (List itf) = "[" ^ itf ^ "]"
    | toString (Option itf) = "Option<" ^ itf ^ ">"
  
  fun toCpp (Interface itf) = itf ^ "*"
    | toCpp (List itf) = "vector<" ^ itf ^ ">"
    | toCpp (Option itf) = itf ^ "*"
end