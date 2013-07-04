
structure SymbolSet = SplaySetFn(struct 
				       type ord_key = Symbol.symbol
				       val compare = Symbol.compare
				   end)

structure TidSet = SplaySetFn(struct 
				  type ord_key = Tid.uid
				  val compare = Tid.compare
			      end)

structure DeadDefinitions =
struct

    open Ast

    structure B = Bindings

    type tidtab = Bindings.tidBinding Tidtab.uidtab

    (* Imperative set operations we rely on. *)
    (* Of course I couldn't find an imperative set so ... *)
    type id_set = SymbolSet.set ref
    fun new_id_set () :id_set = ref SymbolSet.empty 
    fun insert_id (u:id_set) (id:id) =
	(u := SymbolSet.add (!u,#name id))
    fun member_id (u:id_set) (id:id) = 
	SymbolSet.member (!u, #name id)

    type tid_set = TidSet.set ref
    fun new_tid_set () :tid_set = ref TidSet.empty 
    fun insert_tid (u:tid_set) (tid:tid) =
	(u := TidSet.add (!u,tid))
    fun member_tid (u:tid_set) (tid:tid) = 
	TidSet.member (!u, tid)

    datatype 'a cases = Unchanged | Removed | New of 'a

    fun caseJoin fx arg = (case arg of 
			       Unchanged => Unchanged
			     | Removed => Removed
			     | New a => New (fx a))

    fun caseJoin2 fx (arg1,s1) (arg2,s2) rem =
	(case (arg1,arg2) of
	     (Unchanged,Unchanged) => Unchanged
	   | _ => let fun pick s Unchanged = s
			| pick s Removed = rem
			| pick s (New x) = x
		      val s1' = pick s1 arg1
		      val s2' = pick s2 arg2
		  in
		      New (fx (s1',s2'))
		  end)
				    
    fun caseRevMap f l =
	let fun loop [] a = a
	      | loop (hd::tl) (a,unchanged) = 
	    loop tl (case f hd of 
			 Removed => (a,false)
		       | Unchanged => (hd::a,unchanged)
		       | New b => (b::a,false))
	in
	    loop l ([],true)
	end

    fun emptyStmt (STMT (s,a,l)) = STMT(Ast.Compound ([],[]),a,l)

    fun isMain ({name,...}:id) = ((Symbol.name name) = "main")
    fun isHid ({name,...}:id) = String.isPrefix "__H_" (Symbol.name name)

    (* for an expression return true if it has no
     side-effects and can be safely removed. *)
    fun isPureExp (EXPR(ce,_,_)) =
	let val p = isPureExp
	    fun binopOk b =
		(case b of
		     (PlusAssign | MinusAssign | TimesAssign | DivAssign | 
		      ModAssign | XorAssign | OrAssign | AndAssign |
		      LshiftAssign | RshiftAssign | BinopExt _) => false
		    | _ => true)
	    fun unopOk u =
		(case u of
		     (PreInc | PostInc | PreDec | PostDec | UnopExt _) => false
		   | _ => false)
	in
	    case ce of
		(IntConst _ | RealConst _ | StringConst _) => true
	      | Call _ => false
	      | QuestionColon(e1,e2,e3) => p e1 andalso p e2 andalso p e3
	      | Assign _ => false
	      | Comma(e1,e2) => p e1 andalso p e2
	      | Sub(e1,e2) => p e1 andalso p e2
	      | Member(e,_) => p e
	      | Arrow(e,_) => p e
	      | Deref(e) => p e
	      | AddrOf(e) => p e
	      | Binop(b,e1,e2) => binopOk b andalso p e1 andalso p e2
	      | Unop(u,e) => unopOk u andalso p e
	      | Cast(_,e) => p e
	      | Id _ => true
	      | EnumId _ => true
	      | SizeOf _ => true
	      | ExprExt _ => false
	      | ErrorExpr => false
	end

    fun isPureInitExp ie =
	(case ie of
	     Simple e => isPureExp e
	   | Aggregate ies => List.all isPureInitExp ies)

    (* In a backward pass kill variables that are defined but not used. *)
    fun doit (prog:ast, tidtab:tidtab) =
	let val id_set = new_id_set ()
	    val ins_id = insert_id id_set
	    val mem_id = member_id id_set
		
	    val tid_set = new_tid_set ()
	    val ins_tid = insert_tid tid_set
	    val mem_tid = member_tid tid_set

	    fun tidBinding tid = Tidtab.find (tidtab,tid)

	    fun externalDecl (DECL(c,a,l)) = 
		caseJoin (fn x => (DECL(x,a,l))) (coreExternalDecl c)
	    and coreExternalDecl x = 
		(case x of
		     ExternalDecl d => 
			 caseJoin ExternalDecl (declaration true d)
		   | FunctionDef (id,ids,s) => 
			 (doId id;
			  List.app doId ids;
			  case statement s of
			      Unchanged => Unchanged 
			    | Removed => 
				  New (FunctionDef(id,ids,emptyStmt s))
			    | New s' => New (FunctionDef (id,ids,s')))
		   | _ => Unchanged)
	    and declaration isGlobal x = 
		(case x of 
		     VarDecl(id,eopt) =>
			 let fun initializerRemovable eopt =
			     (case eopt of
				  SOME ie => isPureInitExp ie
				| NONE => true)

			     val removable =
			     ((isHid id) andalso
			     (initializerRemovable eopt) andalso
			     (not isGlobal orelse (isGlobal andalso 
						   (#stClass id = STATIC)))
			      orelse (#stClass id = EXTERN andalso isGlobal))
			     val used = mem_id id
			     val prototype = (case #kind id of
						 FUNCTION _ => true
					       | _ => false)

			     fun keep () =
				 ((case eopt of 
				       NONE => ()
				     | SOME e => initExpression e);
				   doId id;
				   Unchanged)
			 in
			     if used then keep()
			     else if prototype then Removed
				  else if removable then Removed
				       else keep ()
			 end
		   | TypeDecl {shadow,tid} =>
			      (if mem_tid tid then (doTid tid; Unchanged)
				   else Removed))
	    and statement (STMT (c,a,l)) = 
		     caseJoin (fn x => STMT (x,a,l)) (coreStatement c)
	    and coreStatement x = 
		(case x of
		     Expr (SOME e) => (expression e; Unchanged)
		   | Compound(ds,ss) =>
			 (* NOTE: Must process statement before declaration. *)
			 let  val (ss', ss_unchanged) =
			     caseRevMap statement (List.rev ss)
			      val (ds',ds_unchanged) = 
				  caseRevMap (declaration false) (List.rev ds)	       
			 in
			     if ds_unchanged andalso ss_unchanged then
				 Unchanged
			     else New (Compound (ds',ss'))
			 end
		   | While(e,s) => 
			 (expression e;
			  caseJoin (fn x => While(e,x)) (statement s))
		   | Do(e,s) => (expression e;
				 caseJoin (fn x => Do(e,x)) (statement s))
		   | For (eo1,eo2,eo3,s) =>
			 (expression_opt eo1;
			  expression_opt eo2;
			  expression_opt eo3;
			  caseJoin (fn x => For(eo1,eo2,eo3,x)) (statement s))
		   | Labeled(l,s) => 
			  caseJoin (fn x => Labeled(l,x)) (statement s)
		   | CaseLabel(l,s) => 
			  caseJoin (fn x => CaseLabel(l,x)) (statement s)
		   | DefaultLabel(s) => caseJoin DefaultLabel (statement s)
		   | Return (SOME e) => (expression e; Unchanged)
		   | IfThen(e,s) => 
			  (expression e;
			   caseJoin (fn x => IfThen(e,x)) (statement s))
		   | IfThenElse(e,s1,s2) => 
			  (expression e;
			   caseJoin2 (fn (x,y) => IfThenElse(e,x,y)) 
			   (statement s1,s1) (statement s2,s2) (emptyStmt s1))
		   | Switch(e,s) => 
			  (expression e;
			   caseJoin (fn x => Switch(e,x)) (statement s))
		   | _ => Unchanged)
	    and expression (EXPR (c,_,_)) = coreExpression c
	    and coreExpression x =
		(case x of 
		     Call (e,es) => (expression e;
				     List.app expression es)
		   | QuestionColon(e1,e2,e3) => (expression e1;
						 expression e2;
						 expression e3)
		   | Assign (e1,e2) => (expression e1;
					expression e2)
		   | Comma (e1,e2) => (expression e1; expression e2)
		   | Sub (e1,e2) => (expression e1; expression e2)
		   | Member (e,_) => expression e
		   | Arrow(e,_) => expression e
		   | Deref(e) => expression e
		   | AddrOf(e) => expression e
		   | Binop(_,e1,e2) => (expression e1; expression e2)
		   | Unop(_,e) => expression e
		   | Cast (ct,e) => (ctype ct; expression e)
		   | Id i => (doId i; ins_id i)
                   | EnumId (m,i) => ctype (#ctype m)
		   | SizeOf ct => ctype ct
		   | _ => () (*end case *))
	    and expression_opt (SOME e) = expression e
	      | expression_opt _ = ()
	    and doId (id:id) = (ctype (#ctype id))
	    and initExpression (Simple e) = expression e
	      | initExpression (Aggregate es) = List.app initExpression es
	    and ctype x =
		(case x of
		     Qual(_,c) => ctype c
		   | Array (SOME (_,e),c) => (expression e; ctype c)
		   | Pointer c => ctype c
		   | Function (c,cs) => (ctype c; List.app ctype cs)
		   | StructRef tid => doTid tid
		   | UnionRef tid => doTid tid
		   | EnumRef tid => doTid tid
		   | TypeRef tid => doTid tid
		   | _ => ())
	    and doTid tid =
		(if mem_tid tid then ()
		 else (ins_tid tid;
		       case tidBinding tid of
			   SOME ({ntype=SOME nc,...}) => namedCtype nc
			 | _ => ()))
	    and namedCtype x = 
		(case x of
		     B.Struct (_,mems) => 
			 (List.app (fn (x,y,z) => (ctype x;
						   case y of 
						       NONE => () 
						     | SOME m => member m))
			  mems)
		   | B.Union (_,mems) => 
			 (List.app (fn (x,y) => (ctype x; member y)) mems)
		   | B.Enum (_,mems) => (List.app (fn (x,y) => member x) mems)
		   | B.Typedef (_,ct) => ctype ct)
	    and member (m:member) = (ctype (#ctype m))
		
	    val (result,unchanged) = caseRevMap externalDecl (List.rev prog)
	in
	    result
	end
end
