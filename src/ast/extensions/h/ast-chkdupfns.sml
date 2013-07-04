structure AstChkDups =
struct

    open Ast

    (* chkdupfns : externalDecl list * TextIO.outstream * string -> bool *)
    fun chkdupfns (decls, outstream, fname) =
	let val errorState = Error.mkErrState outstream
	    fun error loc = Error.error(errorState,loc,
					("Program contains more than one "^
					 "main/sig_main function.\n"))
	    val seenMain = ref false
	    fun check (DECL(FunctionDef(id,_,_),aid,loc)) =
		if Symbol.name(#name(id)) = fname 
		    then if !seenMain then (error loc; false)
			 else (seenMain := true; true)
		else true  
	      | check _ = true		
	in
	  List.all check decls
	end

end

