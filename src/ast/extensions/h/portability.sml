structure Portability =
struct 

    structure P = ParseTreeUtil

    val int8Name = "int8"
    val uint8Name = "uint8"
    val int16Name = "int16"
    val uint16Name = "uint16"
    val int32Name = "int32"
    val uint32Name = "uint32"
    val int64Name = "int64"
    val uint64Name = "uint64"
    val float32Name = "float32"
    val float64Name = "float64"

    val fixedSizedTypes = [int8Name,   (* use a better data structure? *)
                           uint8Name,
			   int16Name,
			   uint16Name,
			   int32Name,
			   uint32Name,
			   int64Name,
			   uint64Name,
			   float32Name,
			   float64Name]

    fun isFixedSize s = List.exists (fn s'=> s = s') fixedSizedTypes

    val uint8CT = P.typedef uint8Name
    val puint8CT = P.ptr uint8CT

    val int8CT = P.typedef int8Name
    val pint8CT = P.ptr int8CT

    val int32CT = P.typedef int32Name

end