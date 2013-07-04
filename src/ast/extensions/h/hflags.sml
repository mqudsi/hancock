structure HFlags :
sig
    val preserveLineNums : bool ref
    val noElim : bool ref
end =
struct

    val preserveLineNums = ref false
    val noElim = ref false
end