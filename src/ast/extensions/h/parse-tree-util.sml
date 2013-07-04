structure ParseTreeUtil =
struct

    structure PT = ParseTree
    structure H = ParseTreeExt
    structure X = HError

    fun makeCT s : PT.ctype = {qualifiers=[], specifiers=s}
    fun makeDT s : PT.decltype = {qualifiers=[],specifiers=s,storage=[]}
    fun ctToDT (ct : PT.ctype) : PT.decltype = 
	let val {specifiers,qualifiers} = ct
	in
	    {specifiers=specifiers,qualifiers=qualifiers,storage=[]}
	end

    fun makeDTstatic (dt : PT.decltype) : PT.decltype =
	{ specifiers = #specifiers dt,
	  qualifiers = #qualifiers dt,
	  storage = [PT.STATIC]}

    fun dtToCT (dt : PT.decltype) =
	{specifiers = #specifiers dt, qualifiers = #qualifiers dt}

    fun removeConst (ct : PT.ctype) : PT.ctype = 
	let val q = if List.exists (fn x => x = PT.VOLATILE) (#qualifiers ct) 
			then [PT.VOLATILE] 
		    else []
	in
	    {qualifiers = q, specifiers = #specifiers ct}
	end

    fun mkConst({qualifiers, specifiers}:PT.ctype) : PT.ctype = 
	{qualifiers=PT.CONST :: qualifiers, specifiers=specifiers}

    val unsigned   = makeCT [PT.Unsigned]
    val char       = makeCT [PT.Char]
    val uchar      = makeCT [PT.Unsigned, PT.Char]
    val uint       = makeCT [PT.Unsigned, PT.Int]
    val int        = makeCT [PT.Int]
    val ushort     = makeCT [PT.Unsigned, PT.Short]
    val short      = makeCT [PT.Short]
    val long       = makeCT [PT.Long]
    val ulong      = makeCT [PT.Long, PT.Unsigned]
    val longlong   = makeCT [PT.Long, PT.Long]
    val ulonglong  = makeCT [PT.Long, PT.Long, PT.Unsigned]
    val float      = makeCT [PT.Float]
    val double     = makeCT [PT.Double]
    val longdouble = makeCT [PT.Long, PT.Double]
    fun ptr ty     = makeCT [PT.Pointer ty]
    val charPtr    = ptr char
    val constCharPtr = mkConst charPtr
    val ucharPtr   = ptr uchar
    val charPtrPtr = ptr charPtr
    fun typedef s  = makeCT [PT.TypedefName s]
    val void       = makeCT [PT.Void]
    val voidPtr    = ptr void
    fun array(e,ct)= makeCT [PT.Array(e,ct)]

    fun func (ret:PT.ctype) (args:PT.ctype list) =
	let fun f ct = (ctToDT ct,PT.EmptyDecr) in	    
	    makeCT [PT.Function {retType = ret, params = List.map f args}]
	end

    fun struc (fields : (string*PT.ctype) list, tag : string option) =
	let fun genField (id,ct) = 
	    (ct,[ (PT.VarDecr id, PT.EmptyExpr) ])
	in
	    makeCT [PT.Struct {isStruct = true,
				 tagOpt = tag,
				 members = List.map genField fields
				 }
		      ]
	end

    fun structDecl (fields : (string*PT.ctype) list, tag : string option) =

        PT.ExternalDecl(
          PT.Declaration(
              ctToDT(struc(fields,tag)),
              []))

    fun structTagCT name = 
	{qualifiers=[],
	 specifiers=[PT.StructTag
		     {isStruct=true,name=name}]}


    fun intX i = PT.IntConst (IntInf.fromInt i)
    fun int32X i = (PT.IntConst (IntInf.fromInt (Int32.toInt i)))
    fun int32X i = (PT.IntConst i)

    val zero = intX 0

    val trueX             = intX 1
    val falseX            = zero

    fun addrX e           = PT.Unop(PT.AddrOf,e)
    fun andX (e1,e2)      = PT.Binop(PT.And,e1,e2)
    fun arrowX (e1,e2)    = PT.Binop(PT.Arrow,e1,e2)
    fun assignX (lhs,rhs) = PT.Binop(PT.Assign,lhs,rhs)
    fun assignS (lhs,rhs) = PT.Expr(assignX (lhs,rhs))
    fun commaX (e1,e2)    = PT.Binop(PT.Comma,e1,e2)
    fun dotX (lhs,rhs)    = PT.Binop(PT.Dot,lhs,rhs)
    fun eqX (e1,e2)       = PT.Binop(PT.Eq,e1,e2)
    fun neqX (e1,e2)      = PT.Binop(PT.Neq,e1,e2)
    fun ltX (e1,e2)       = PT.Binop(PT.Lt,e1,e2)
    fun lteX(e1,e2)       = PT.Binop(PT.Lte,e1,e2)
    fun gtX (e1,e2)       = PT.Binop(PT.Gt,e1,e2)
    fun notX e            = PT.Unop(PT.Not,e)
    fun orX (e1,e2)       = PT.Binop(PT.Or,e1,e2)
    fun plusX (e1,e2)     = PT.Binop(PT.Plus,e1,e2)
    fun minusX (e1,e2)    = PT.Binop(PT.Minus,e1,e2)
    fun timesX (e1,e2)    = PT.Binop(PT.Times,e1,e2)
    fun rshiftX (e1,e2)   = PT.Binop(PT.Rshift,e1,e2)
    fun modX (e1,e2)      = PT.Binop(PT.Mod,e1,e2)
    fun postIncX e        = PT.Unop(PT.PostInc, e)
    fun postDecX e        = PT.Unop(PT.PostDec, e)
    fun starX e           = PT.Unop(PT.Star,e)
    fun sizeofX ct        = PT.Unop(PT.SizeofType(ct),PT.EmptyExpr)
    fun sizeofEX e        = PT.Unop(PT.Sizeof, e)
    fun strIsNonNull name = PT.Binop(PT.Neq,PT.Id name,zero)
    fun subX(e1,e2)       = PT.Binop(PT.Sub, e1, e2)
    fun compoundS ss      = PT.StatExt(H.HCompound (PT.Compound ss)) 
    val emptyS            = (PT.Expr PT.EmptyExpr)
    fun returnS e         = PT.Return(e) 
    fun condX (e1,e2,e3)  = PT.QuestionColon(e1,e2,e3)
	
    (* create a comma separated expression from a list. *)
    fun commaXs [] = PT.EmptyExpr
      | commaXs [hd,tl] = commaX(hd,tl)
      | commaXs (hd::tl) = commaX(hd,commaXs tl)

    (* WARNING: mkFunctionEDecl always generates a static function. *)
    fun mkFunctionEDecl(funName, paramList, bodyS, retTy) =
           PT.FunctionDef
              {body = bodyS,
               funDecr = PT.FuncDecr(PT.VarDecr funName, paramList),
               krParams = [],
               retType = makeDTstatic retTy}


    (* parse-tree representations of min and max values for each chars,
       unsigned chars, shorts, unsigned shorts, ints, and unsighed ints. *)
    fun umax sMax = plusX(timesX(sMax, intX 2),intX 1)

    val charMax = PT.Cast(
			  char,
			  rshiftX((*  (((unsigned)-1) >> 1) *)
				  PT.Cast(uchar, intX ~1),
				  intX 1))
    val ucharMax = umax charMax
    val charMin = PT.Unop(PT.Negate,charMax)

    val shortMax = PT.Cast(
                      short,
                      PT.Binop(                (*  (((unsigned)-1) >> 1) *)
                         PT.Rshift,
                         PT.Cast(ushort, intX ~1),
                         intX 1))
    val ushortMax = umax shortMax
    val shortMin = PT.Unop(PT.Negate,shortMax)

    val intMax = PT.Cast(
                      int,
                      PT.Binop(                (*  (((unsigned)-1) >> 1) *)
                         PT.Rshift,
                         PT.Cast(unsigned, intX ~1),
                         intX 1))
    val uintMax = umax (PT.Cast(longlong,intMax))
    val intMin = PT.Unop(PT.Negate,intMax)

(* (* Use  &(((t *)0)->foo)  to calculate offset of field foo in struct type t. *)
    fun offsetX(cStructPtr,fieldName) =
	PT.Cast(int,
	        addrX(
      	          arrowX(PT.Cast(cStructPtr, zero),
                           PT.Id fieldName)))

    fun sizeofTypX(cStructPtr, fieldName) =
	sizeofEX(arrowX(PT.Cast(cStructPtr, zero),
                           PT.Id fieldName))

    fun commentS s = 
	let val len = String.size s
	    val line = 50
	    val space = (line-len-4)
	    val prefix = Int.div(space,2)
	    val filler = #"*" 	
	    fun padLeft s = StringCvt.padLeft filler (prefix+len) s
	    fun padRight s = StringCvt.padRight filler (space+len) s
	in
	    if space<0 then PT.CommentStmt ("\n"^s^"\n")
	    else 
		PT.CommentStmt (padRight (padLeft s))
	end

   (* Builds an assignable l-value for variable paramName with type ty.
      We use this form to cast away the const-ness of paramName. *)
   fun param (ty, paramX) =
         PT.Cast(
            ptr ty,
            addrX(paramX))

    fun genId (name:string) = (PT.Id name, PT.VarDecr name)

    (* WARNING: funDef always declares a static function. *)
    fun funDef (name  : string,
		  args  : (string * PT.ctype) list,
		  retCT : PT.ctype,
		  body  : PT.statement ) =
	let fun argsToDecr (arg,ct) = (ctToDT ct,PT.VarDecr arg)
	in
	    (* The storage on the return type determines the storage class 
	     of the function. *)
	PT.FunctionDef{
		       funDecr = PT.FuncDecr(PT.VarDecr name,
					     List.map argsToDecr args),
		       krParams = [],
		       retType = makeDTstatic (ctToDT retCT),
		       body = body
		       }
	end

    fun alval (ty, paramX) = starX(param(ty,paramX))

    (* runtime expression to determine whether the c compiler
     * assumes that char is unsigned or signed: (char) 0 < (char) 255 . *)
    val charIsUnsignedX = ltX(PT.Cast(char,
					  zero),
				  PT.Cast(char,
					  intX 255))

    fun isAssignOp (PT.Assign | PT.PlusAssign | PT.MinusAssign | 
		      PT.TimesAssign | PT.DivAssign | PT.ModAssign | 
		      PT.XorAssign | PT.OrAssign | 
		      PT.AndAssign | PT.LshiftAssign | PT.RshiftAssign) = true
      | isAssignOp _ = false

    fun isDecl (PT.Decl _) = true
      | isDecl _ = false
	
    fun assignOpModifier PT.Assign = NONE
      | assignOpModifier x = 
	SOME (case x of
		  PT.PlusAssign => PT.Plus
		| PT.MinusAssign => PT.Minus
		| PT.TimesAssign => PT.Times
		| PT.DivAssign => PT.Divide
		| PT.ModAssign => PT.Mod
		| PT.XorAssign => PT.BitXor
		| PT.OrAssign => PT.BitOr
		| PT.AndAssign => PT.BitAnd
		| PT.LshiftAssign => PT.Lshift
		| PT.RshiftAssign => PT.Rshift
		| _ => X.bug ("Unexpected operator."))
		  
    (* The converse of the above function. *)
    fun assignModifierOp NONE = PT.Assign
      | assignModifierOp (SOME x) =
	(case x of
	     PT.Plus => PT.PlusAssign
	   | PT.Minus => PT.MinusAssign
	   | PT.Times => PT.TimesAssign
	   | PT.Divide => PT.DivAssign
	   | PT.Mod => PT.ModAssign
	   | PT.BitXor => PT.XorAssign
	   | PT.BitOr => PT.OrAssign
	   | PT.BitAnd => PT.AndAssign
	   | PT.Lshift => PT.LshiftAssign
	   | PT.Rshift => PT.RshiftAssign
	   | _ => X.bug ("Unexpected operator."))

    fun declS (dt : PT.decltype,
		 decr : PT.declarator,
		 init : PT.expression) : PT.statement =
	PT.Decl(PT.Declaration (dt,[(decr,init)]))

    fun varDeclS (ct,v,init) = declS(ctToDT ct,PT.VarDecr v,init)
    fun varDeclS' (ct,v) = varDeclS(ct,v,PT.EmptyExpr)



    fun ctDeclS (ct,decr,init) = declS(ctToDT ct,decr,init)

    (* Declares global variables as static. *)
    fun Edecl (dt : PT.decltype,
		 decr : PT.declarator,
		 init : PT.expression) : PT.externalDecl =
	PT.ExternalDecl(PT.Declaration (makeDTstatic dt,[(decr,init)]))

    fun varEDecl (ct,v,init)   = Edecl(ctToDT ct,PT.VarDecr v,init)
    fun ctEDecl (ct,decr,init) = Edecl(ctToDT ct,        decr,init)


    fun varArrayDeclS(ct,arrayName,arraySize) =
        declS(ctToDT ct,
                PT.ArrayDecr(PT.VarDecr arrayName, intX arraySize), 
                PT.EmptyExpr)


    fun extractMode (specs:PT.specifier list) : 
	(H.Hmode * PT.specifier list) =
	let fun isMode (PT.SpecExt H.HNew)    = true
	      | isMode (PT.SpecExt H.HExists) = true
	      | isMode _ = false
	    val (modes,specs') = List.partition isMode specs
	in
	    case modes of 
		[] => (H.Either,specs)
	      | [PT.SpecExt H.HExists] => (H.Exists, specs')
	      | [PT.SpecExt H.HNew]    => (H.New   , specs')
	      | _ => (X.error "More than one new or exists present in type.";
		      (H.Either,specs))
	end

    fun extractModeCT (ct: PT.ctype) : (H.Hmode * PT.ctype) = 
	let val {specifiers,qualifiers} = ct
	    val (mode,specs') = extractMode specifiers
	in
	    (mode,{specifiers = specs', qualifiers=qualifiers})
	end

    (* FMS: XXX Should all this event related code be here?  
     Perhaps it would be better in cnv-ext.sml *)

    fun mkEvtFlag s = s^"_f"
    fun mkEvtValue s = s^"_v"
    fun mkEvtFlagId s = PT.Id (mkEvtFlag s)
    fun mkEvtValueId s = PT.Id (mkEvtValue s)

    fun evtFlagX t f = dotX(t, mkEvtFlagId f)
    fun initEventFlagX t f b = 
	assignX (evtFlagX t f, if b then intX 1 else zero)
    fun evtValueX t f = dotX(t, mkEvtValueId f)
    fun initEventValueX t f x = assignX (evtValueX t f, x)

    fun evtHX mkB t [] = X.bug "event name must have at least one string.\n"
      | evtHX mkB t [n] = mkB t n
      | evtHX mkB t (n::ns) = evtHX mkB (dotX(t, mkEvtValueId n)) ns

    fun evtFlagHX t ns = evtHX evtFlagX t ns
    fun evtValueHX t ns = evtHX evtValueX t ns

    fun decrToVar (dr:PT.declarator) = 
	(case dr of 
	     PT.VarDecr s            => SOME s
	   | PT.ArrayDecr(dr,e)      => decrToVar dr
	   | PT.PointerDecr dr       => decrToVar dr
	   | PT.QualDecr(q,dr)       => decrToVar dr
	   | PT.FuncDecr(dr,_)       => decrToVar dr
	   | PT.MARKdeclarator(_,dr) => decrToVar dr
	   | PT.DecrExt dx =>
		 (case dx of
		      H.HScope(_,s)     => SOME s
		    | H.HWindow(dr,_,_) => decrToVar dr
		    | H.HParamApp(dr,_) => decrToVar dr)
	   | _                       => NONE)

    fun isExpPure e =
	(case e of
	     PT.Id _ => true
	   | PT.Binop(oper,e1,e2) => 
		 (case oper of
		      (PT.Dot | PT.Arrow) => isExpPure e1
		    | PT.Sub => (isExpPure e1 andalso isExpPure e2)
		    | _ => false)
	   | (PT.IntConst _ | PT.RealConst _ | PT.String _) => true
	   | PT.Cast(ct,e) => (isExpPure e)
	   | PT.MARKexpression(er',e) => (isExpPure e)
	   | _ => false)


    fun isExpAddressable e =
	(case e of
	     PT.Id _ => true
	   | PT.MARKexpression(er',e) => (isExpAddressable e)
(*	   | PT.Cast(ct,e) => (isExpAddressable e) *)
	   | _ => false)

    fun isConst PT.CONST = true
      | isConst _ = false

    fun isEmptyS s = 
	(case s of
	    PT.Expr PT.EmptyExpr => true
	  | PT.Compound [] => true
	  | PT.MARKstatement(l,s) => isEmptyS s
	  | PT.StatExt(H.HCompound(PT.Compound [])) => true
	  | _ => false)


    fun prependS ([]:PT.statement list) (s:PT.statement) : PT.statement = s
      | prependS ss1 (PT.Compound ss2) = (PT.Compound (ss1 @ ss2))
      | prependS ss1 (PT.MARKstatement (l,s)) = 
	(PT.MARKstatement(l,prependS ss1 s))
      | prependS ss1 s = (compoundS (ss1 @ [s]))
	
    fun extractTyName ({specifiers,...}:PT.ctype) : string option =
	let fun f [] = NONE
	      | f ((PT.TypedefName s)::rs) = SOME s
	      | f (r::rs) = f rs
	in 
	    f specifiers 
	end

    fun isStructOrUnion ({specifiers,...}:PT.ctype) : bool =
	let fun chk (PT.Struct _) = true
              | chk _ = false
	in
          List.exists chk specifiers
	end

    fun mkParam(ct,s) = (ctToDT ct, PT.VarDecr s)

    (* ksf: the way this code gets the parse tree types for directory fields is
     broken.  When moving to newer version, upgrade to use ct to pt routine. *)
	
	
    fun getTempDecr s PT.EmptyDecr = PT.EmptyDecr
      | getTempDecr s PT.EllipsesDecr = PT.EllipsesDecr
      | getTempDecr s (PT.VarDecr old) = PT.VarDecr s
      | getTempDecr s (PT.ArrayDecr ((PT.VarDecr old), e)) = 
	(PT.VarDecr s) 
      | getTempDecr s (PT.ArrayDecr (d,e)) = 
	PT.ArrayDecr((getTempDecr s d),e)
      | getTempDecr s (PT.PointerDecr d) = 
	(PT.PointerDecr (getTempDecr s d))
      | getTempDecr s (PT.QualDecr (q,d)) = 
	PT.QualDecr(q,(getTempDecr s d))
      | getTempDecr s (PT.FuncDecr (_)) = 
	X.bug "Unimplemented 1.\n"
      | getTempDecr s (PT.MARKdeclarator (l,d)) = 
	PT.MARKdeclarator(l,getTempDecr s d)
      | getTempDecr s (PT.DecrExt (H.HParamApp(d,es))) = 
        getTempDecr s d
      | getTempDecr s (PT.DecrExt (_)) = 
	X.bug "Unimplemented 2.\n"
	
    fun isArrayDecr (PT.ArrayDecr(d,e)) = true
      | isArrayDecr _ = false
	
    fun isPtrDecr (PT.PointerDecr(d)) = true
      | isPtrDecr _ = false

    fun pathToString [] = ""
      | pathToString [n] = n
      | pathToString (n::ns) = n^"."^(pathToString ns)

    fun initParams  (condV : PT.expression, baseX:PT.expression, 
		     l: (PT.ctype * string) list, noGuard : bool) =
	let fun doList l = 
	    let fun initParam (ct,n) = 
		let val initX = arrowX(baseX, PT.Id n)
		in
		    (varDeclS(ct,n,PT.InitList[zero]), assignS(PT.Id n, initX))
		end
		val stms = List.map initParam l
		val (decls, initSs) = ListPair.unzip stms
		val bodyS = if noGuard 
			    then (compoundS ((commentS "Function parameter means parameters must be supplied.")
					     :: initSs))
			    else PT.IfThen(condV, compoundS initSs)
	    in
		decls @[bodyS]
	    end
	in
	    case l of [] => [] | _ => doList l
	end


    fun declareParams (l: (PT.ctype * string) list) =
	let fun initParam (ct,n) = varDeclS(ct,n,PT.InitList[zero])
	in
            List.map initParam l
	end

    fun initParamStruct (baseX:PT.expression) (l: PT.expression list) =
	let fun initOne e = 
	    let val lhsX = dotX(baseX, e)
	    in
		assignS(lhsX, e)
	    end
	in
	    List.map initOne l
	end

    fun initParamStruct2 (baseX:PT.expression) (nl: string list) (il: PT.expression list) =
	let fun initOne (n,i) = 
	    let val lhsX = dotX(baseX, PT.Id n)
	    in
		assignS(lhsX, i)
	    end
	in
	    ListPair.map initOne (nl,il)
	end

    fun dependsOn (nl:string list) (exp:PT.expression) =
	let fun depends exp = 
	    case exp
	      of PT.EmptyExpr => false
	      |  PT.IntConst (i) => false
	      |  PT.RealConst (r) => false
	      |  PT.String (s) => false
	      |  PT.Id(s) => List.exists (fn ps => s = ps) nl
	      |  PT.Unop(rator,e) => depends e
	      |  PT.Binop(rator,e1,e2) => (depends e1) orelse (depends e2)
	      |  PT.QuestionColon(e1,e2,e3) => (depends e1) orelse (depends e2) orelse (depends e3)
	      |  PT.Call(funexp, exps) => (depends funexp) orelse (List.exists depends exps)
	      |  PT.Cast (ct, e) => depends e
	      |  PT.InitList exps => List.exists depends exps
	      |  PT.MARKexpression (s,e) => depends e
	      |  PT.ExprExt(ext) => false (* XXX should we allow hancock forms ?*)
	in
	    depends exp
	end
end

