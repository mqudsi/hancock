(* XXX - should substitute through declarators too This case only
 matters if the expression contains a sizeof a type with complex
 declarators containing an array mentioning the variable we are
 substituting for.  Should clean this up sometime. (Why do we need
 both substitution functions?)  

 Specifier extensions are proably wrong too.
 *) 

structure HParseTreeSubst : H_PARSE_TREE_SUBST = struct

    open ParseTree
    open ParseTreeExt

    fun fieldArgOp oper = 
	(case oper of
	     (Dot | Arrow) => true
	   | OperatorExt x => 
		 (case x of
		      (HDot | HArrow | HRemove ) => true
		    | _ => false)
	   | _ => false)

    fun substExp (id,e,exp) =
	let fun s' exp = substExp (id,e,exp)
	in
	    case exp of
		Id s => if (id=s) then e else exp
	      | Binop(oper,e1,e2) => 
		    if fieldArgOp oper then
			Binop(oper,s' e1,e2)
		    else 
			Binop(substOper (id,e,oper),s' e1,s' e2)
              | Unop(oper, e) => Unop(oper, s' e)
	      | QuestionColon(e1,e2,e3) => QuestionColon(s' e1, s' e2, s' e3)
	      | Call (e,es) => Call(s' e,map s' es)
	      | Cast(ct,e) => Cast(ct,s' e)
	      | InitList es => InitList (map s' es)
	      | MARKexpression (l,e) => MARKexpression(l,s' e)
	      | ExprExt x => (substExpExt (id,e,x))
	      | _ => exp
	end
    and substExpExt (id,e,x) =
	let fun s' exp = substExp (id,e,exp)
	    fun ss' (s,SOME x) = (s,SOME (s' x))
	      | ss' (s,NONE)   = (s,NONE)
	in
	    ExprExt (case x of 
			 HAssign e' => HAssign (s' e')
		       | HEventC elts => HEventC (List.map ss' elts)
		       | HEventEmpty x => HEventEmpty x)
	end	    
    and substOper (id,e,x) =
	(case x of
	     SizeofType(ct) => SizeofType(substCT(id,e,ct))
	   | _ => x)
    and substCT (id,e,{qualifiers=q,specifiers=s}) =
	{qualifiers=q,
	 specifiers=List.map (fn x => substSpec (id,e,x)) s}
    and substSpec (id,e,x) =
	let fun se' x = substExp(id,e,x)
	    fun sct' x = substCT(id,e,x)
	in
	    case x of
		Array(e',ct) => Array(substExp (id,e,e'),
				      substCT(id,e,ct))
	      | Pointer(ct) => Pointer(substCT (id,e,ct))
	      | Enum {tagOpt,enumerators,trailingComma} =>
		    Enum {tagOpt=tagOpt,
			  enumerators = List.map (fn (s,e) => (s,se' e)) enumerators,
			  trailingComma=trailingComma}
	      | Struct {isStruct,tagOpt,members} =>
		    let fun g (d,e) = (d,se' e)
			fun f (ct,des) = (sct' ct, 
					  List.map g des)
		    in 
			Struct {isStruct = isStruct,
				tagOpt = tagOpt,
				members = List.map f members
				}
		    end
	      | _ => x
	end

    fun isFreeInExp (vars : string list, EmptyExpr )  = false
      | isFreeInExp (vars : string list, IntConst _ )  = false
      | isFreeInExp (vars : string list, RealConst _ )  = false
      | isFreeInExp (vars : string list, String _ )  = false		       
      | isFreeInExp (vars : string list, Id s )  = List.exists (fn s'=> s = s') vars
      | isFreeInExp (vars : string list, Unop (rator,exp) )  = isFreeInExp(vars,exp)
      | isFreeInExp (vars : string list, Binop (rator,exp1,exp2) )  = 
	isFreeInExp(vars,exp1) orelse isFreeInExp(vars,exp2)
      | isFreeInExp (vars : string list, QuestionColon (exp1,exp2,exp3) )  = 
	isFreeInExp(vars,exp1) orelse isFreeInExp(vars,exp2) orelse isFreeInExp(vars,exp3)
      | isFreeInExp (vars : string list, Call (exp1,exps) )  = 
	isFreeInExp(vars,exp1) orelse (List.exists (fn e => isFreeInExp(vars,e)) exps)
      | isFreeInExp (vars : string list, Cast (ty,exp) )  = isFreeInExp(vars,exp)
      | isFreeInExp (vars : string list, InitList exps )  = 
         List.exists (fn e => isFreeInExp(vars,e)) exps
      | isFreeInExp (vars : string list, MARKexpression(l, exp)) = isFreeInExp(vars,exp)
      | isFreeInExp (vars : string list, ExprExt e) = isFreeInExpExt(vars, e)

    and isFreeInExpExt (vars, HAssign exp) = isFreeInExp(vars, exp)
      | isFreeInExpExt (vars, HEventC elts) = 
	    let fun ss' (s,SOME e) = (List.exists (fn s' => s = s') vars) orelse 
		                     isFreeInExp(vars,e)
	          | ss' (s,NONE)   = (List.exists (fn s' => s = s') vars) 
            in 
		List.exists ss' elts
	    end
      | isFreeInExpExt (vars, HEventEmpty ct) = false
end
