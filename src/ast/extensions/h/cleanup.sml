
(* Called to do cleanup after the ast has been generated. *)
structure Cleanup : 
sig 
    type tidtab = Bindings.tidBinding Tidtab.uidtab

    val doit : (Ast.ast * tidtab) -> Ast.ast
end =
struct

    open Ast

    type tidtab = Bindings.tidBinding Tidtab.uidtab

    fun isLineDirective s = String.isPrefix "#line" s

    (* There are many redundant directives. *)
    fun elimRedundantDirectives(ast) =
	let fun loop [] = []
	      | loop ((hd as DECL(Directive s1,_,_)) ::
		      (tl as DECL(Directive s2,_,_) :: tl2)) = 
	    if isLineDirective s1 andalso isLineDirective s2 then loop tl
	    else hd :: (loop tl)
	      | loop (hd::tl) = hd :: (loop tl)
	in
	    loop ast
	end

    fun doit(ast:ast,tidtab:tidtab) =
	let 
	    val ast = 
	    if !HFlags.noElim then ast else DeadDefinitions.doit (ast,tidtab)
	    val ast = 
		if !HFlags.preserveLineNums then elimRedundantDirectives ast
		else ast
	in
	    ast
	end
    
end
