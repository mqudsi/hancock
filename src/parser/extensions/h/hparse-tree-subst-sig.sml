signature H_PARSE_TREE_SUBST =
sig
    val substExp: (string * ParseTree.expression * ParseTree.expression) ->
	ParseTree.expression

    val isFreeInExp : (string list * ParseTree.expression) -> bool

end
