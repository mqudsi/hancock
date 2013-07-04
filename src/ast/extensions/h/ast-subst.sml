structure AstSubst =
struct

    open Ast

    fun substExp (id:id,e) (exp as EXPR(ce,aid,loc)) : expression =
	let val s' = substExp (id,e)
	    fun w' ce = EXPR (ce,aid,loc)
	in
	    case ce of
		Id (i:id) =>
		    (if Symbol.equal (#name id, #name i) then e
		    else exp)
	      | Call (e,es) => 
		    w' (Call (s' e,List.map s' es))
	      | QuestionColon (e1,e2,e3) =>
		    w' (QuestionColon (s' e1, s' e2, s' e3))
	      | Assign (e1,e2) =>
		    w' (Assign (s' e1, s' e2))
	      | Comma (e1,e2) => 
		    w' (Comma (s' e1, s' e2))
	      | Sub (e1,e2) =>
		    w' (Sub (s' e1, s' e2))
	      | Member (e,m) =>
		    w' (Member (s' e, m))
	      | Arrow (e,m) =>
		    w' (Arrow (s' e, m))
	      | Deref e =>
		    w' (Deref (s' e))
	      | AddrOf e =>
		    w' (AddrOf (s' e))
	      | Binop (b,e1,e2) =>
		    w' (Binop (b,s' e1, s' e2))
	      | Unop (u,e) =>
		    w' (Unop (u,s' e))
	      | Cast (c,e) =>
		    w' (Cast (c,s' e))
	      | _ => exp
	end
end