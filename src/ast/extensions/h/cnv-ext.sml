(* Table of Contents

   Structures                 - structure declarations
   Exceptions                 - exception declarations
   Types                      - type definitions/abbreviations
   Imported Values            - values imported from other modules
   Error Reporting            - functions for error reporting
   Hancock Utilities          - hancock specific utilities
   ParseTree                  - functions for manipulating/generating PT's
   Ast                        - functions for manipulating/generating AST's
   Conversions                - Functions that perform the actual conversions.
      sig_main
      iterate
      expressions
      binary operators
      external declarations
      declarators
      unchanged

 Prologue:

 Coding conventions (for this file):
    Modules are never opened.  They may be renamed, but the new name must be
    used consistently.

    Any use of stateful computation should be commented with (* STATE : ...*)

    Wrong or potentially incorrect code should be commented with (* XXX : ...*)

    Personal comments are inserted with (* initials : ...*)

 Naming conventions:
    Top-level variables optionally begin with an all caps prefix indicating 
    the section they come from.  The first word (following the prefix) is
    lowercase with subsequent words capitalized.

    Some common abbreviations are:
        CT or  ct for   ctype
        DT or  dt for decltype
	 X        for expression
	 S        for statment

 Type encoding:
    Hancock types are ALL compiled to typedef's.  Hancock information is 
    associated with the tid in the hancock table.  One consequence of this
    choice is that variables of hancock typs can be used as their typedef'd
    C type.

    Because circularities between modules are disallowed, there is a H.ctype
    which is exactly the same but not equal to PT.ctype. 

*)
structure CnvExt : CNVEXT = 
struct

(* Structures ****************************************************************)

    structure PT   = ParseTree     (* the parse tree *)
    structure H    = ParseTreeExt  (* parse tree extensions *)
    structure P    = ParseTreeUtil (* parse tree utilities. *)
    structure CL   = CLib          (* the C library *)
    structure HRS  = HRS           (* the Hancock Runtime System *)
    structure POR  = Portability   (* interface to partability code *)
    structure HS   = HState        (* state maintained by hcc *)
    structure HI   = HInfo         (* information associated with types. *)
    structure HPTS = HParseTreeSubst 
    structure SM   = SourceMap
    structure SYM  = Symbol
    structure B    = Bindings
    structure TU   = TypeUtil
    structure PPL  = PPLib
    structure S    = State
    structure AS   = AstSubst
    structure AST  = Ast
    structure X    = HError
    structure HL   = HLib  (* generic utilities. *)

    structure SymSet = BinarySetFn(struct type ord_key = SYM.symbol 
					  val compare = SYM.compare end)

    structure Sort = ListMergeSort

(* Exceptions ****************************************************************)

    exception CnvExt of string
    exception Abort (* always handled *)

(* Types *********************************************************************)

    type ctype = Ast.ctype
 
    type coreConversionFuns = 
	{
	 stateFuns : State.stateFuns,
	 mungeTyDecr: (Ast.ctype*ParseTree.declarator ->Ast.ctype * string option),
	 
	 cnvType : bool*ParseTree.decltype -> Ast.ctype*Ast.storageClass,
	 cnvExpression: ParseTree.expression -> Ast.ctype * Ast.expression,
	 cnvStatement : ParseTree.statement -> Ast.statement,
	 cnvExternalDecl: ParseTree.externalDecl -> Ast.externalDecl list,
	 
	 wrapEXPR: (Ast.ctype*Ast.coreExpression -> Ast.ctype*Ast.expression),
	 wrapSTMT: Ast.coreStatement -> Ast.statement,
	 wrapDECL: Ast.coreExternalDecl -> Ast.externalDecl,
	 evalExpr: ParseTree.expression -> 
	              (IntInf.int option * Ast.ctype * Ast.expression * bool)
	 }

    type extensionFuns = 
	{
	 CNVExp: ParseTree.expressionExt -> Ast.ctype * Ast.expression,
	 CNVStat: ParseTree.statementExt -> Ast.statement,
	 CNVBinop: ({binop: ParseTreeExt.operatorExt, 
		     arg1Expr: ParseTree.expression, 
		     arg2Expr: ParseTree.expression}
		    -> Ast.ctype * Ast.expression),
	 CNVUnop: ({unop: ParseTreeExt.operatorExt, 
		    argExpr: ParseTree.expression}
		   -> Ast.ctype * Ast.expression),
	 CNVExternalDecl: ParseTree.externalDeclExt -> Ast.externalDecl list,
	 CNVSpecifier: ({isShadow: bool, rest : ParseTree.specifier list} 
			-> ParseTree.specifierExt
			-> Ast.ctype),
	 CNVDeclarator: (Ast.ctype * ParseTree.declaratorExt 
			 -> Ast.ctype * string option),
	 CNVDeclaration: ParseTree.declarationExt -> Ast.declaration list}

    type HEvent  = (PT.ctype,PT.declarator,PT.statement) H.HEvent
    type HIterates  = (PT.ctype,PT.declarator,PT.expression,
		       PT.statement) H.HIterates
    type HRecord = (PT.ctype,PT.declarator, PT.statement) H.HRecord
    type HSigParam = (PT.ctype,PT.declarator,PT.expression) H.HSigParam
    type HPath = H.HPath

    type exp = PT.expression
    type stmt = PT.statement
    type cty  = PT.ctype

(* The Function **************************************************************)
(* makeExtensionFuns wraps the remaining code in this file! 
   This is needed to put stateFuns in scope. *)
    fun makeExtensionFuns ({stateFuns,
			   mungeTyDecr,
			   cnvType,
			   cnvExpression,
			   cnvStatement,
			   cnvExternalDecl,
			   wrapEXPR,
			   wrapSTMT,
			   wrapDECL,
			   evalExpr}:coreConversionFuns) =
    let val cnvStmt = cnvStatement
	val cnvExp = cnvExpression
	val wrapExp = wrapEXPR
	val wrapStmt = wrapSTMT
	val wrapDecl = wrapDECL

(* Imported Values ***********************************************************)
    val {locFuns =
	 {pushLoc, popLoc, getLoc, error, warn},
	 tidsFuns =
	 {pushTids, resetTids},
	 envFuns =
	 {topLevel, pushLocalEnv, popLocalEnv, lookSym, bindSym,
	  lookSymGlobal, bindSymGlobal, lookLocalScope, getGlobalEnv},
	 uidTabFuns =
	 {bindAid, lookAid=lookAid0, bindTid, lookTid},
	 funFuns =
	 {newFunction, getReturnTy, checkLabels, addLabel, addGoto}, 
	 switchFuns =
	 {pushSwitchLabels, popSwitchLabels, addSwitchLabel, addDefaultLabel},
	 ...}
	= stateFuns

    val ttab = (#ttab (#uidTables (#globalState stateFuns)))

    val isFunction          = TU.isFunction          ttab
    val isStructOrUnion     = TU.isStructOrUnion     ttab
    val isConst             = TU.isConst             ttab
    val isIntegral          = TU.isIntegral          ttab
    val isEquable           = TU.isEquable           ttab
    val getCoreType         = TU.getCoreType         ttab
    val getFunction         = TU.getFunction         ttab
    val equalType           = TU.equalType           ttab
    val hasKnownStorageSize = TU.hasKnownStorageSize ttab

    fun sizeOf cty = #bytes (Sizeof.byteSizeOf
			     {sizes=Sizes.defaultSizes,
			      err= X.error, warn=X.warn, bug=X.bug} ttab cty)
	

    fun isAssignable (t1,t2,rhsOpt) = 
	let val isRhs0 = 
	    case rhsOpt of
		SOME (Ast.EXPR (Ast.IntConst i, _, _)) => i = IntInf.fromInt 0
	      | _ => false
	in
	    (TU.isAssignable ttab {lhs = t1, rhs = t2, rhsExpr0 = isRhs0 })
	end

    type pcty = ParseTree.ctype

    fun ASTid(sym:SYM.symbol,
	      ct:Ast.ctype,
	      isGlobal:bool,
	      status : Ast.declStatus) : Ast.id =
	{name = sym,
	 uid = Pid.new (),
	 location = getLoc (),
	 ctype = ct,
	 stClass = if isGlobal then Ast.STATIC else Ast.DEFAULT,
	 status = status, 
	 global = isGlobal,
	 kind = (if (isFunction ct) then 
		     (Ast.FUNCTION {hasFunctionDef = false})
		 else Ast.NONFUN)
	 }

    fun ASTlocalId (s, ct) = ASTid(s, ct, false, Ast.DEFINED)

    fun localInitVar (id:string, ct:Ast.ctype) : SYM.symbol * Ast.id =
	let val sym = Symbol.object id
	    val id = ASTlocalId(sym, ct)
	in
	    bindSym (sym, B.ID id);
	    (sym, id)
	end

    fun insTempVar (id, pcty:pcty) = 
        let val (acty, sc) = cnvType (false, P.ctToDT pcty)
	in
	    localInitVar(id, acty)
	end



(* Error Reporting  **********************************************************)

	(* Setup the error reporting. *)
	val _ = (X.setup (#errorState (#globalState stateFuns)) 
		 (#error (#locFuns (stateFuns))) 
		 (#warn  (#locFuns (stateFuns))))

	fun ifParseError e = (case e of SOME e => X.fail e | NONE => ())

(* Hancock Utilities *******************************************************)
    val seenSigMain = ref false  (* for detecting multiple sig_mains *)
    fun intInftoInt32 i = Int32.fromInt(IntInf.toInt i)
    val Hid = HRS.Hid 

    fun unbound sym = (case lookSym sym of
			   SOME _ => X.fail ("Redeclaration of " ^ 
					   (Symbol.name sym))
			 | NONE   => ())
 
    (* determine whether all symbols in a list are distinct. 
     Return a duplicate if one exists. *)
    fun distinctSymbols (ss: SYM.symbol list) : SYM.symbol option =
	let fun loop ([],set) = NONE
	      | loop (hd::tl,set) = 
	    (if SymSet.member (set,hd) then (SOME hd)
	     else loop (tl,SymSet.add(set,hd)))
	in
	    loop (ss,SymSet.empty)
	end
 
    (* Bind member to a new tid. *)
    fun rebindMember ({name,uid,location,ctype,kind}:Ast.member,
		      tid,
		      ct:ctype) =
	let val name' = SYM.name (name)
	    val sym' = SYM.member (tid,name')
	    val m' = {name = name, 
		      uid = Pid.new (), 
		      location = getLoc (), 
		      ctype = ct,
		      kind = kind}
	in
	    bindSym (sym',B.MEMBER m');
	    m'
	end


    fun CTtoString (ct:Ast.ctype) =  
	let val underscore = !PPL.suppressTidUnderscores
	    val          _ =  PPL.suppressTidUnderscores := true
	    val        str =  PPL.ppToString (PPAst.ppCtype () ttab) ct
	    val          _ =  PPL.suppressTidUnderscores := underscore
	in 
	    str 
	end

    fun isGlobalFunction(f:string, errMsg:string) =
        case (lookSymGlobal (SYM.symbol{name=f,kind=SYM.FUNCTION}))
        of NONE => (X.error errMsg)
        | _ => ()


    fun isGenerativeStream (HI.Stream {isGenerative,...}) = isGenerative
      | isGenerativeStream _ = false
(* Parse Tree ****************************************************************)

    (* internal variables. *)
    fun PTinternalVar(var,ct,initX) =
	let val v = Hid var
	    val i = PT.Id v
	    val d = case initX of 
		NONE => P.varDeclS'(ct,v)
	      | SOME init => P.varDeclS(ct,v,init)
	in
	    (v,i,d)
	end

    fun PTisConstExp e = 
	case e of 
	    PT.EmptyExpr => (X.bug "EmptyExpression passed to PTisConstExp")
          |  PT.RealConst _ => true   (*XXX- ksf: generalize this to constant real expressions. *)
          | _ => (case evalExpr e of
		     (SOME _,_,_,false) => true
		   | _ => false)

    fun PTisZero e = 
	case e of 
	    PT.EmptyExpr => false
          | _ => (case evalExpr e of
		     (SOME i,_,_,false) => (i=IntInf.fromInt 0)
		   | _ => false)

   fun cnvStaticInt(exp) = 
      (case  exp of
         PT.EmptyExpr => (X.bug "EmptyExpression passed to cnvStaticInt.")
         | exp => (case evalExpr exp 
		       of (SOME i, _,_,_) => ((IntInf.toInt i) handle OverFlow => 
				 (X.error "Window specification too large."; 0))
		     | _ => (X.error ("Window specifications must evaluate to a constant"); 0)))



    fun PTisConstInit e =
	case e of
	    PT.MARKexpression(_,e) => PTisConstInit e
	  | PT.InitList es => List.all PTisConstInit es
	  | PT.EmptyExpr => false
	  | _ => PTisConstExp e


    (* Given two side-effect-free expressions (hopefully just
       variables) of type ctype, generate an expression to test them
       for equality. Works for structure types as well. *)
    fun PTequalX (e1:PT.expression,
		  e2:PT.expression,ct:ctype) : PT.expression =
	let fun structEqual tid =
	    let fun dot e f = P.dotX(e,PT.Id f)
		val {name,ntype,global,location} = 
		case lookTid tid of
		    NONE => X.bug "PTequalX: unbound Tid"
		  | SOME b => b
	    
		val mems = 
		    case ntype of 
			NONE => X.bug "PTequalX: struct with partial type."
		      | SOME (B.Struct (_,mems)) => mems
		      | SOME _ => X.bug "PTequalX: expected a struct!"
		fun gen (ct',SOME ({name=f,...}:Ast.member),NONE) = 
		    let val f = SYM.name f in
			PTequalX(dot e1 f,dot e2 f,ct')
		    end
		    | gen _ = X.bug "PTequalX: bit-fields, unnamed field unimplemented."
		val conjuncts = List.map gen mems
	    in
		List.foldr P.andX P.trueX conjuncts
	    end
	in
	    case isEquable {ty1=ct,exp1Zero=false,
			    ty2=ct,exp2Zero=false} of
		SOME _ => P.eqX(e1,e2)
	      | NONE => 
		    (case getCoreType ct of
			 (Ast.Void | Ast.Ellipses) => 
			     X.fail ("Cannot compare values of type " 
				   ^ (CTtoString ct))
		       | Ast.Array (NONE,ct') =>
			     X.fail ("Cannot compare arrays of unknown size.")
		       | Ast.Array(SOME(li,e'),ct') =>
			     X.bug "PTequalX: Fixed size arrays unimplemented"
		       | Ast.StructRef tid => structEqual tid
		       | Ast.UnionRef tid => (* XXX *)
			     X.bug "PTequalX: Unions unimplemented"
		       | Ast.EnumRef tid => (* XXX *)
			     X.bug "PTequalX: Enum unimplemented"
		       | (Ast.Pointer _ | Ast.Function _ | Ast.Numeric _) =>
			     X.bug "PTequalX: isEquable should succeed here!"
		       | (Ast.TypeRef _ | Ast.Qual _) =>
			     X.bug "PTequalX: getCoreType failed."
		       | Ast.Error => X.bug "PTequalX : getCoreType failed")
	end


   fun checkString param exp =
       let val lval = P.alval(P.charPtr, param)
	   val sizeX = P.timesX(P.plusX(CL.strlen(exp),
					P.intX 1), 
				P.sizeofX P.char)
	   val mallocX = PT.Cast(P.charPtr, CL.malloc sizeX)
	   val checkS = PT.IfThen(P.eqX(param, P.zero),
				  HRS.errorS("malloc failed."))
	   val copyS = PT.Expr(CL.strcpy(lval, exp))
	   val bodyS = P.compoundS [P.assignS(lval,mallocX),
				    checkS,
				    copyS]
       in
	    bodyS
       end


(* Ast ***********************************************************************)

    fun ASTisExpPure (AST.EXPR (ce,_,_)) =
	(case ce of
	     (Ast.IntConst _ | Ast.RealConst _ | Ast.StringConst _ |
	      Ast.Id _) => true
	     | Ast.Member(e,_) => ASTisExpPure e
	     | Ast.Arrow (e,_) => ASTisExpPure e
	     | _ => false
	     )

    fun ASTid(sym:SYM.symbol,
	      ct:Ast.ctype,
	      isGlobal:bool,
	      status : Ast.declStatus) : AST.id =
	{name = sym,
	 uid = Pid.new (),
	 location = getLoc (),
	 ctype = ct,
	 stClass = if isGlobal then Ast.STATIC else Ast.DEFAULT,
	 status = status, 
	 global = isGlobal,
	 kind = (if (isFunction ct) then 
		     (Ast.FUNCTION {hasFunctionDef = false})
		 else Ast.NONFUN)
	 }
	
    fun ASTlocalId (s,ct) = ASTid(s,ct,false,Ast.DEFINED)
    fun ASTglobalId(s,ct) = ASTid(s,ct,true, Ast.DEFINED)

    (* Bind a PT variable with name name and type ct in the global
     symbol table as declared but not yet defined.  
     Only works for functions or constants, not typedefs. *)
    fun bindGlobal (name:string, ct:ctype) =
	let val sym = SYM.object name
	    val id = ASTid(sym,ct,true,Ast.DECLARED)
            val binding = B.ID id
	in
	    bindSymGlobal (sym,binding)
	end

    (* Bind the symbols declared in the declaration list. *)
    fun bindDecl (d : Ast.declaration) =
	(case d of 
	     Ast.VarDecl(id,_) => bindSym (#name id, B.ID id)
	   | _ => ())
	
    fun bindDecls (ds : Ast.declaration list) = List.app bindDecl ds
	
    fun declTemp (s:string,
		  ct:ctype):(string * SYM.symbol * Ast.id * Ast.declaration) =
	let val t = Hid s
	    val sym = SYM.object t
	    val id = ASTlocalId (sym,ct)
	    val decl = Ast.VarDecl(id,NONE)
	in
	    (t,sym,id,decl)
	end

    val Astzero =  Ast.EXPR((Ast.IntConst(IntInf.fromInt 0)),Aid.new(),SM.UNKNOWN)

    fun declZeroedTemp (s,ct) =
	let val (t,sym,id,decl) = declTemp (s,ct) 
	    val zeroInit = Ast.Aggregate [Ast.Simple Astzero] (* XXX - does this work??? *)
	    val decl' = 
		case decl of
		    Ast.VarDecl (id,NONE) =>
			Ast.VarDecl(id,SOME zeroInit)
	       | _ => X.bug "Definition of declTemp changed!"
	in
	    (t,sym,id,decl')
	end

    fun declBindTemp (name:string,ct:ctype,isZeroed:bool) : string = 
	let val (res,_,_,d) = if isZeroed then declZeroedTemp(name,ct) 
			      else declTemp(name,ct)
	in
	    bindDecl d;
	    HS.addDecl d;
	    res
	end
	    (* Binds the symbol to the id *)
    fun localInitVar (id:string,ct:Ast.ctype) : SYM.symbol * Ast.id =
	let val sym = Symbol.object id
	    val id = ASTlocalId(sym,ct)
	in
	    bindSym (sym,B.ID id);
	    (sym,id)
	end

    fun declTid tid = 
	wrapDecl (Ast.ExternalDecl (Ast.TypeDecl {shadow=NONE,
						  tid=tid})) 

    (* FMS: Copied verbatim from build-ast.sml function isZeroExp *)
    fun AstIsZero(Ast.EXPR(Ast.IntConst i,_,_)) = i = (IntInf.fromInt 0)
      | AstIsZero _ = false


    fun ASTStructTid bindSym sym tid ct =
	let val symBinding = {name     = sym,
			      uid      = Pid.new (),
			      location = getLoc (),
			      ctype    = Ast.TypeRef tid }
	    val tidBinding = {name     = SOME (SYM.name sym),
			      ntype    = SOME (B.Struct(tid,[(ct, NONE,NONE)])),
			      location = getLoc (),
			      global   = true } (* FMS: XXX - should always be global? *)
	in
	    bindSym (sym,B.TAG symBinding);
	    bindTid (tid,tidBinding);
	    ()
	end

    fun ASTtypedefTid bindSym sym tid ct =
	let val symBinding = {name     = sym,
			      uid      = Pid.new (),
			      location = getLoc (),
			      ctype    = Ast.TypeRef tid }
	    val tidBinding = {name     = SOME (SYM.name sym),
			      ntype    = SOME (B.Typedef(tid,ct)),
			      location = getLoc (),
			      global   = true } (* FMS: XXX - should always be global? *)
	in
	    bindSym (sym,B.TYPEDEF symBinding);
	    bindTid (tid,tidBinding);
	    ()
	end


    (* Typedefs name to be ct.  Returns the related tid. Guarantees
     that this name is not previously typedef'd *)
    fun ASTtypedefGen bSym (name:string,ct:Ast.ctype): Tid.uid =
	let val sym = Symbol.typedef name
	    val _ = unbound sym
	    val tid = Tid.new ()
	    val symBinding = {name     = sym,
			      uid      = Pid.new (),
			      location = getLoc (),
			      ctype    = Ast.TypeRef tid }
	    val tidBinding = {name     = SOME name,
			      ntype    = SOME (B.Typedef(tid,ct)),
			      location = getLoc (),
			      global   = true } (* FMS: XXX - should always be global? *)
	in
	    bSym (sym,B.TYPEDEF symBinding);
	    bindTid (tid,tidBinding);
	    tid
	end
    val ASTtypedef =ASTtypedefGen bindSym
    val ASTtypedefGlobal = ASTtypedefGen bindSymGlobal

(* Ctype *********************************************************************)

    val CTint = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.INT,
			     Ast.SIGNDECLARED)
    val CTchar = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.CHAR,
			     Ast.SIGNASSUMED)
    val CTlonglong = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,
			     Ast.LONGLONG, Ast.SIGNDECLARED)

    datatype CTsign = Signed | Unsigned | Any
    type CTnum =  Ast.intKind * CTsign

    fun CTgetNum ct =
	(case getCoreType ct of
	     Ast.Numeric(_,_,s', ik', _) => SOME (ik', s')
	   | _ => NONE)

    fun CTisNum (ik,s) ty =
	(case getCoreType ty of
	     Ast.Numeric(_, _, s', ik', _) => 
		 (if ik' = ik then
		     (case (s,s') of 
			  (Any     , _           ) => true
			| (Signed  , Ast.SIGNED  ) => true
			| (Unsigned, Ast.UNSIGNED) => true
			| _ => false)
		  else false)
	   | _ => false)
	
    val CTisChar  = CTisNum (Ast.CHAR,Any)
    val CTisSChar = CTisNum (Ast.CHAR,Signed)
    val CTisUChar = CTisNum (Ast.CHAR,Unsigned)

    val CTisShort  = CTisNum (Ast.SHORT,Any)
    val CTisSShort = CTisNum (Ast.SHORT,Signed)
    val CTisUShort = CTisNum (Ast.SHORT,Unsigned)

    val CTisInt  = CTisNum (Ast.INT,Any)
    val CTisSInt = CTisNum (Ast.INT,Signed)
    val CTisUInt = CTisNum (Ast.INT,Unsigned)

    val CTisLong  = CTisNum (Ast.LONG,Any)
    val CTisSLong = CTisNum (Ast.LONG,Signed)
    val CTisULong = CTisNum (Ast.LONG,Unsigned)

    val CTisLongLong  = CTisNum (Ast.LONGLONG,Any)
    val CTisSLongLong = CTisNum (Ast.LONGLONG,Signed)
    val CTisULongLong = CTisNum (Ast.LONGLONG,Unsigned)

    val CTisFloat  = CTisNum (Ast.FLOAT,Any)
    val CTisDouble  = CTisNum (Ast.DOUBLE,Any)
    val CTisLongDouble  = CTisNum (Ast.LONGDOUBLE,Any)

    val CTisPointer = TU.isPointer ttab
    val CTisArray   = TU.isArray ttab
    val CTisNumber  = TU.isNumber ttab

    fun CTisVoid ty = (case (getCoreType ty)
                       of Ast.Void => true
		       | _ => false)

    fun CTarrayInfo ty =
	(case (getCoreType ty) of
	     (Ast.Array(sizeexpopt, cty)) => SOME(sizeexpopt,cty)
           | _ => NONE)

    fun CTisFixedSizeArray ty =
	case (CTarrayInfo ty) of 
             NONE => false
           | SOME(NONE,_) => false
	   | SOME(SOME _,_) => true


    fun CTisString ty = 
        let val coreTy = getCoreType ty
            val isPointer = CTisPointer coreTy
            fun getBase coreTy =
                let val derefTyOpt = TU.deref ttab coreTy
                in
		    case derefTyOpt
		    of SOME(baseTy) => baseTy
                    | _ => X.bug "Impossible: must be able to dereference a pointer.\n"
                end
        in
            isPointer andalso (CTisChar (getBase coreTy))
        end


    fun CTisStruct ty = 
	case isStructOrUnion ty
	    of SOME tid => 
		(case lookTid tid of
		     SOME {ntype = SOME(B.Struct(_)),...} => true
		   | _ => false)
	  | NONE => false
		     
    fun CTgetMemberTys tid = 
	let val l = 
	    case lookTid tid
		of SOME {ntype = SOME(B.Struct(tid',l)),...} =>l
	      | _ => X.bug "Expected struct.\n"
	in
	    map (fn (ct,_,_)=> ct) l
	end


    fun CTgetMemberNames' l = 
	let fun extractName NONE = 
	             (X.error "Hancock supports only named fields in structs.\n";
	              "Error")
	      | extractName (SOME (member:Ast.member)) = SYM.name (#name member)
	    val memOpts = map (fn (_,memOpt,_)=> memOpt) l
	in
	    map extractName memOpts
	end

    fun CTgetMemberNames tid = 
	let val l = case lookTid tid
	            of SOME {ntype = SOME(B.Struct(tid',l)),...} =>l
		  | _ => X.bug "Expected struct.\n"
	in
	    CTgetMemberNames' l
	end

    fun CTgetMemberNamesAndTys tid =
	let val l = case lookTid tid of
	                 SOME {ntype = SOME(B.Struct(tid',l)),...} => l
		       | _ => X.bug "Expected struct.\n"
            val names = CTgetMemberNames' l
	    val tys = map (fn(ct,_,_) => ct) l
	in
	    ListPair.zip (names, tys)
	end


    fun CTcnvType (ct : PT.ctype) : (ctype * Ast.storageClass) 
	= cnvType(false,P.ctToDT ct)

    fun CTcnvDecr(ct,d) = 
	let val (ct', sc) = CTcnvType ct  	(* check storage class okay*)
	in                                 	(* XXX - missing piece *)
	    mungeTyDecr(ct',d)
	end

    fun CTtempDeclArg (ct,d) =
	let val (ct', nopt) = CTcnvDecr(ct,d)
	in
	    case nopt of
		NONE => ()
	      | SOME(n) => ignore(localInitVar(n,ct'))
	end



    fun CTfromTypeName (s:string) : ctype = 
	let val sym = Symbol.typedef s 
	    val binding = lookSym sym
	in
	    case binding of
		SOME (B.TYPEDEF tidInfo) =>
		    (#ctype tidInfo)
	      | _ => X.bug (s^" is not bound to a typedef.")
	end

    fun CTfromTypeName' (s:string, errMsg:string) : ctype = 
	let val sym = Symbol.typedef s
	    val binding = lookSym sym
	in
	    case binding of
		SOME (B.TYPEDEF tidInfo) =>
		    (#ctype tidInfo)
	      | _ => X.fail (s^ " " ^ errMsg)
	end

    fun CTreduceTypeDef tid =
       let val (nameopt, ntypeopt) = (case lookTid tid of
			  	        SOME {name,ntype,...} => (name,ntype)
				      | NONE => X.bug "Expected bound tid.\n")
           val name = case nameopt of 
	                   SOME name => name
			 | NONE => X.bug "Expected type name.\n"
	   val nct = case ntypeopt of 
	                  SOME(B.Typedef(tid,ct)) => ct 
	                | _ => X.bug "Expected typedef.\n"
			      
       in
	   (name,nct)
       end


    (* Type-utils implements but does not export an essentially identical
     function!!! *)
    fun CTreduce ct = 
	(case ct of 
	     Ast.TypeRef tid =>
		 (case lookTid tid of
		      SOME {ntype = SOME (B.Typedef (_,ct)),...} => 
			  (CTreduce ct)
		    | NONE => X.bug "Ill-formed type table."
		    | _ => ct)
	   | _ => ct)


    (* given a ctype it returns the innermost tid
     or NONE if none exists. *)
    (* Warning: This could cycle if the typedef table is ill-formed. *)
    fun CTinnerTid (ct:ctype) =
	let fun stripCT ct =
	    case ct of
	        Ast.TypeRef tid =>
		    (case lookTid tid of
			 SOME {ntype=SOME (B.Typedef(_,ct)), 
			       ... } => SOME(tid,ct)
		       | SOME _ => NONE
		       | _ => X.bug "Completely unbound tid.")
	      | _ => NONE
			 
	    fun loop (tid,ct) =
		case stripCT ct of
		    NONE => tid
		  | SOME (tid,ct) => loop (tid,ct)
	in
	    case stripCT ct of
		NONE => NONE
	      | SOME (tid,ct) => SOME (loop (tid,ct))
	end

    (* given a ctype searches through all typedefs until it finds one with
     hancock information or bottoms out.*)
    (* Previous implementation that looked for hancock information on the 
     innermost tid does not work on maps. *)
    fun CThinfo ct = 
	(case ct of 
             Ast.Qual(qual,ct) => CThinfo ct
	   | Ast.TypeRef tid =>
		 (case HS.lookId tid of 
		      NONE => 
			  (case lookTid tid of
			       SOME {ntype=SOME (B.Typedef(_,ct)),
				     ... } => (CThinfo ct)
				   | SOME _ => NONE
				   | NONE => X.bug "Completely unbound tid. (2)")
		    | d => d)
	   | _ => NONE)

    fun CTtypedefName ct =
	(case ct of 
	     Ast.Qual(_,ct) => CTtypedefName ct
	   | Ast.TypeRef tid => 
		 (case lookTid tid of
		      SOME {name=SOME n,...} => SOME n
		    | _ => NONE)
	   | _ => NONE)

    (* Check whether this ctype is a record and if it is 
     a record check that it is not one of the views.*)
    fun CTrecord ct : HI.RecordInfo option=
	case CThinfo ct of
	    SOME(HI.Record r) =>
		let val {v1Name,v2Name,...} = r 
		    val name = 
			case CTtypedefName ct of
			    SOME n => n
			  | NONE => ""
		in
		    if (name = v1Name orelse
			name = v2Name) then
			NONE
		    else SOME r
		end
	  | _ => NONE
		
    fun CTmapInfo ct =
	(case CThinfo ct 
	   of SOME (HI.Map mi) => mi
           |  SOME (HI.Param{basety,exprs}) => CTmapInfo basety
	   | _ => X.fail ("Expected a map type, but found " ^
			CTtoString ct))
    fun CTeventInfo ct =
	(case CThinfo ct of
	     SOME (HI.EventTy ei) => ei
	   | _ => X.fail ("Expected an munion, but found " ^
			CTtoString ct))
	     
    fun tidName tid : string option =
	(case lookTid tid of
	     NONE => X.bug "Unbound tid."
	   | SOME {name = sn,...} => sn)
	     
    datatype structTree = 
	Node of (string * ctype option * structTree) list |
	Leaf of ctype

 (* The following should be combined with CTstructFieldNames since
 they are so similar*)
    fun CTstructMembers msg (ct:ctype) : (Tid.uid * ((ctype*Ast.member) list))=
	(case isStructOrUnion ct of
	     SOME tid =>
		 (case lookTid tid of
		      SOME {ntype = SOME (B.Struct (_,cals)),...} =>
			  let fun f (c,SOME m,NONE) = (c,m)
				| f (c,_,SOME _) = 
			      X.bug "Struct has unexpected bit-field."
				| f (c,NONE,_) =
			      X.bug "Struct has unnamed field."
			  in
			      (tid,List.map f cals)
			  end
		    | SOME {ntype = NONE,...} =>
			  X.bug "Undefined type name."
		    | SOME _ =>
			  X.bug "Expected a structure."
		    | NONE  => 
			  X.bug "Undefined structure.")
	   | NONE => X.bug ("Expected a structure here." ^ msg))


    (* The following function "decompiles" a ctype.  We need this to generate
     the type of the event argument. *)
    fun CTtoPTct (ct:ctype) : PT.ctype =
	(case ct of
	     Ast.Void => P.void
	   | Ast.Ellipses => P.makeCT [PT.Ellipses]
	   | Ast.Qual (q,ct') => 
		 let val q' = (case q of 
				   Ast.CONST => PT.CONST 
				 | _ => PT.VOLATILE)
		     val {qualifiers=q'',specifiers = s''} = CTtoPTct ct'
		 in
		     { qualifiers = q' :: q'',
		       specifiers = s''
		       }
		 end		     
	   | Ast.Numeric(s,f,sgn,intk,sgntag) => 
		 let val sat = (case s of 
				    Ast.SATURATE => PT.Saturate 
				  | _ => PT.Nonsaturate)
		     val frac = (case f of 
				     Ast.FRACTIONAL => PT.Fractional
				   | Ast.WHOLENUM => PT.Wholenum)
		     fun cnvSgn Ast.SIGNED = [PT.Signed]
		       | cnvSgn Ast.UNSIGNED = [PT.Unsigned]
		     val sgn = (case sgntag of
				            Ast.SIGNASSUMED => []
     				          | Ast.SIGNDECLARED => cnvSgn sgn)
		     val ik = (case intk of
				   Ast.CHAR => [PT.Char]
				 | Ast.SHORT => [PT.Short]
				 | Ast.INT => [PT.Int]
				 | Ast.LONG => [PT.Long]
				 | Ast.LONGLONG => [PT.Long, PT.Long]
				 | Ast.FLOAT => [PT.Float]
				 | Ast.DOUBLE => [PT.Double]
				 | Ast.LONGDOUBLE => [PT.Long, PT.Double])
		     val specs = sat :: frac :: sgn @ ik
		 in
		     P.makeCT specs
		 end
	   | Ast.Array (iopt,ct') =>
		 let val e = (case iopt of 
				  NONE => PT.EmptyExpr
				| SOME (i,_) => P.int32X i) (* XXX: should get expression but it is an AST expression. *)
		     val ct'' = CTtoPTct ct'
		 in
		     P.makeCT [PT.Array(e,ct'')]
		 end
	   | Ast.Pointer ct' => P.makeCT [PT.Pointer (CTtoPTct ct')]
	   | Ast.Function (ct',cts) =>
		 let val ct'' = CTtoPTct ct'
		     fun f ct = (P.ctToDT (CTtoPTct ct),PT.EmptyDecr)
		 in
		     P.makeCT [ PT.Function { retType = ct'',
					      params = (List.map f cts)
					      } ]
		 end
	   | Ast.StructRef t => 
		 let fun procMem (ct,mopt : Ast.member option,iopt) =
		     let val ct' = CTtoPTct ct 
			 val dr = 
			     case mopt of
				 NONE => PT.EmptyDecr
			       | SOME {name,...} => PT.VarDecr (SYM.name name)
			 val e = 
			     case iopt of
				 NONE => PT.EmptyExpr
			       | SOME i => P.int32X i
		     in
			 (ct',[(dr,e)])
		     end
		 in case lookTid t of
(*		     SOME {name=SOME n,ntype=NONE,...} =>
			 P.makeCT [PT.StructTag {isStruct=true, name=n }] *)
		     SOME {name=SOME n,...} =>
			 P.makeCT [PT.StructTag {isStruct=true, name=n }] 
		   | SOME {name=nopt,ntype=SOME (B.Struct (_,ms)), ...} =>
			 P.makeCT [PT.Struct {isStruct=true,
					      tagOpt=nopt,
					      members=List.map procMem ms}]
		   
		   | _ => X.bug "Ill-formed type table (struct)."
		 end
	   | Ast.UnionRef t => 
		 let fun procMem (ct,m:Ast.member) =
		     let val ct' = CTtoPTct ct
			 val dr = PT.VarDecr (SYM.name (#name m))
		     in
			 (ct',[(dr,PT.EmptyExpr)])
		     end
		 in case lookTid t of
		     SOME {name=SOME n,ntype=NONE,...} => 
			 P.makeCT [PT.StructTag {isStruct=false, name=n}]
		   | SOME {name=nopt,ntype=SOME (B.Union (_,ms)),...} =>
			 P.makeCT [PT.Struct {isStruct = false,
					      tagOpt = nopt,
					      members = List.map procMem ms}
				    ]			 
		   | _ => X.bug "Ill-formed type table (union)."
		 end
	   | Ast.EnumRef t =>
		 let fun procMem ({name,...}:Ast.member,i) = 
		     (SYM.name name, P.int32X i)
		 in case lookTid t of
		     SOME {name=SOME n,ntype=NONE,...} => 
			 P.makeCT [PT.EnumTag n]
		   | SOME {name=nopt,ntype=SOME (B.Enum (_,ms)),...} =>
			 P.makeCT [PT.Enum {tagOpt = nopt,
					    enumerators = List.map procMem ms,
					    trailingComma = false}]
		   | _ => X.bug "Ill-formed type table (enum)."
		 end
	   | Ast.TypeRef t =>
		 let in case lookTid t of
		     SOME {name= SOME n,...} => P.makeCT [PT.TypedefName n]
		   | _ => X.bug "Ill-formed type table (typedef)."
		 end
	   | Ast.Error => X.fail "Error type found."
	     )


    fun CTDeclToPTDecl (a:Ast.declaration) : PT.declaration =
	case a of
	    AST.TypeDecl _ => X.bug "CTDeclToPTDecl: Unexpected1."
	  | AST.VarDecl (id,SOME _) => X.bug "CTDeclToPTDecl: Unexpected2."
	  | AST.VarDecl (id,NONE) =>
		let val ct = #ctype id
		    val name = SYM.name (#name id)
		    val dt = P.ctToDT (CTtoPTct ct)
		in
		    PT.Declaration (dt,[(PT.VarDecr name,PT.EmptyExpr)])
		end		    

    (* get parse-tree list of argument name, type; put parameters scope first. *)
    fun mungeTyParams name p = let val (pct,nOpt) = CTcnvDecr p
				   val pName =  
				       case nOpt 
					   of NONE => (X.error ("Parameter to " ^name^
								" must have a name."); "")
					 | SOME n => n
				   val _ = localInitVar(pName,pct)(* put params in scope *)
				   val PTpct = CTtoPTct pct
			       in 
				   (PTpct,pName)
			       end



(* Portable Compression ******************************************************)

   exception VariableSize;

   fun compress(e, bp, name) = 
		[PT.Expr(PT.Call(PT.Id ("store_"^name),
                                 [P.addrX bp, e]))]
   fun decompress(e, bp, name) =
		[PT.Expr(PT.Call(PT.Id ("read_"^name),
		                 [P.addrX bp, P.addrX e]))]

   fun portTypeRef (tid, e, bp, doBasic) =
       let val (name,nct) = CTreduceTypeDef tid
       in
	   if (POR.isFixedSize name) then 
	       (P.sizeofX(P.typedef name),
	        doBasic(e, bp, name))
	   else
	       portAST(nct, e, bp, doBasic)
       end

   and portStruct(tid, e, bp, doBasic) =
       let val namesAndTys = CTgetMemberNamesAndTys tid
	   fun doOne ((name,nct), (aSize, cnvSs)) = 
	   let val (nSize, ncnvSs) = 
                    portAST(nct, P.dotX(e,PT.Id name), bp, doBasic) 
	   in
	       (P.plusX(aSize,nSize), ncnvSs@cnvSs)
	   end
       in
	   List.foldl doOne (P.zero, []) namesAndTys 
       end

   and portArray(iopt, ct, e, bp, doBasic) =
       let    
           val numX = case iopt of 
                        SOME (i, numX)  => P.int32X i
                      | NONE => ((X.error ("Cannot generate "^
		                          "compression functions for "^
                                          "maps with arrays of unspecified "^
					  "sizes.\n")); P.zero)
           val index = PT.Id "index"					 
           val (elem_size, elem_pack) = 
	             portAST(ct, P.subX(e,index), bp, doBasic)
           val size = P.timesX(numX, elem_size)
           val cnvS = P.compoundS(
                        [P.varDeclS'(P.int, "index"),
                         PT.For(P.assignX(index, P.zero),
                                P.ltX(index, numX),
				P.postIncX index,
				P.compoundS(elem_pack))])
       in
          (size, [cnvS])
       end

   and portAST (ct : Ast.ctype, e : PT.expression, 
                  bp : PT.expression, doBasic)
	   : (PT.expression * PT.statement list) = 
       case ct of
	   Ast.Qual(q,ct') => portAST  (ct', e, bp, doBasic)
	 | Ast.StructRef t => portStruct(t, e, bp, doBasic) 
	 | Ast.EnumRef t => raise VariableSize (* enums are ints *)
	 | Ast.TypeRef t => portTypeRef(t, e, bp, doBasic)
	 | Ast.Array(iopt,ct) => portArray(iopt, ct , e, bp, doBasic)
         | _ => raise VariableSize;


   fun genFunCompress(f) = 
       fn(ps,pd,bu,v,l,cSize) =>
         P.assignS(PT.Id cSize, 
		   PT.Call(f,[PT.Id v, PT.Id bu, PT.Id l]))

   fun genParamFunCompress(f,exprs,typedParams,hasFunctionParam) = 
       fn(ps,pd,bu,v,l,cSize) => 
          let val initPs = P.initParams (PT.Id ps,PT.Id pd, typedParams, hasFunctionParam)
	      val callUserCompX = PT.Call(PT.Id f,
					    [PT.Id ps] @ exprs @ [PT.Id v, PT.Id bu, PT.Id l])
	      val callUserCompS = P.assignS(PT.Id cSize, callUserCompX)
	  in
              PT.Compound(initPs @ [callUserCompS])
	  end


   (* Return a function that computes statements necessary to do default
      compression for type ct from variables (represented as strings)
      representing the signed buffer pointer, unsigned buffer pointer,
      the value to be compressed, and the resulting size. *)
   fun genDefaultCompress(ct) = 
       let val (act,_) = CTcnvType ct
       in
          fn(ps,pd,bu,v,l,cSize) => 
	    let val (sz, cnvSs) = portAST(act, P.starX (PT.Id v), PT.Id "bs", compress)
		              handle VariableSize => 
				let val sz = P.sizeofX(ct) 
				in (sz, [(PT.Expr (CL.memcpy (PT.Id "bs",PT.Id v, sz)))])
				end
	    in
	       PT.Compound ([P.varDeclS(POR.pint8CT, "bs", PT.Cast(POR.pint8CT, PT.Id bu))]
			    @ cnvSs
			    @ [P.assignS(PT.Id "cSize", sz)])
	    end
       end

   fun mapStripeCompress(name, cf, rangeCT,  exprs,
			 typedParams, paramCT, hasFunctionParam) = 
       let val wrapperName = Hid("stripe_c_wrapper_"^name)
	   val ps = Hid("paramSet")
	   val pd = Hid("paramData")
	   val stripe = Hid("stripe")
	   val sl = Hid("stripeLen")
	   val cdata = Hid ("cdata")
	   val cdataExt = Hid("cdataExtent")
           val params = [(P.ctToDT (P.char),    PT.VarDecr ps),
			 (P.ctToDT (P.ptr paramCT), PT.VarDecr pd),
			 (P.ctToDT (P.ptr rangeCT) , PT.VarDecr stripe),
			 (P.ctToDT POR.int32CT, PT.VarDecr sl),
			 (P.ctToDT POR.puint8CT,PT.VarDecr cdata),
			 (P.ctToDT POR.int32CT, PT.VarDecr cdataExt)]
           val initPs = P.initParams (PT.Id ps,PT.Id pd,
				      typedParams,hasFunctionParam)
	   val paramSet = if 0 = (List.length exprs) then [] else [PT.Id ps]
	   val callUserCompX = PT.Call(cf,
				       paramSet @ exprs @ [PT.Id stripe, PT.Id sl, 
							   PT.Id cdata, PT.Id cdataExt])
	   val returnS = PT.Return(callUserCompX)
	   val bodyS = PT.Compound(initPs @[returnS])
	   val edecl = P.mkFunctionEDecl(wrapperName,params,bodyS,P.ctToDT POR.int32CT)
       in
	   (wrapperName, edecl)
       end

   (* generate wrapping for entry compression function for map name with value
      type ct.  Use funtion genCompS to generate statements to do actual
      compression.  For defaults, uses genDefaultCompress(ct).  For functions,
      uses raw function or parameterized function. *)
   fun mapCompress (name,ct, paramCT, genCompS) =
       let val fname = Hid ("compress_"^name)
	   val params = [(P.ctToDT (P.char),    PT.VarDecr "paramSet"),
			 (P.ctToDT (P.ptr paramCT), PT.VarDecr "paramData"),
			 (P.ctToDT (P.ptr ct) , PT.VarDecr "v"),
			 (P.ctToDT POR.puint8CT,PT.VarDecr "bu"),
			 (P.ctToDT POR.int32CT, PT.VarDecr "l")]
           val sizDecl = P.varDeclS(P.int, "cSize", P.zero)
	   val compressS = genCompS("paramSet", "paramData","bu","v","l","cSize")
	   val checkS = PT.IfThenElse(
			 P.gtX(PT.Id "cSize",PT.Id "l"),
		         HRS.errorS (name^": Packing is too inefficient.\n"),
			 PT.IfThenElse(
			  P.eqX(PT.Id "cSize", P.intX ~1), (* HRS_ERROR *)
			  HRS.errorS (name^": Packing failed.\n"),
			  PT.IfThenElse(
			   P.eqX(PT.Id "cSize", P.zero),
			   HRS.errorS (name^": Packed value to 0 bytes.  Return "^
				     "HRS_REMOVEENTRY to remove an entry on compression.\n"),
                           PT.IfThen(
			    P.andX(P.ltX(PT.Id "cSize", P.zero), P.neqX(PT.Id "cSize", P.intX ~2)), 
				     (* HRS_REMOVEENTRY is -2*)
			    HRS.errorS2(name^": pack returned undefined value %d.\n",  [PT.Id "cSize"], "")
                           )
                          )
                         )    
			)
	   val returnS = P.returnS (PT.Id "cSize")
	   val bodyS = PT.Compound [sizDecl,compressS, checkS, returnS]
       in
	   (fname,P.mkFunctionEDecl(fname,params,bodyS,P.ctToDT POR.int32CT))
       end


   (* Return a function that computes statements necessary to do default
      decompression for type ct from variables (represented as strings)
      representing the signed buffer pointer, unsigned buffer pointer,
      the value to be compressed, and the resulting size. *)
   fun genDefaultDecompress(ct) = 
       let val (act,_) = CTcnvType ct
       in
          fn(ps,pd,bu,v,l,cSize) => 
	    let val (sz, cnvSs) = portAST(act, P.starX (PT.Id v), PT.Id "bs", decompress)
		              handle VariableSize => 
				let val sz = P.sizeofX(ct) 
				in (sz, [(PT.Expr (CL.memcpy (PT.Id v,PT.Id "bs",sz)))])
				end
	    in
	       PT.Compound ([P.varDeclS(POR.pint8CT, "bs", PT.Cast(POR.pint8CT, PT.Id bu))]
			    @ cnvSs
			    @ [P.assignS(PT.Id "cSize", sz)])
	    end
       end

   fun genFunDecompress(f) = 
       fn(ps,pd,bu,v,l,cSize) =>
         P.assignS(PT.Id cSize, 
		   PT.Call(f,[PT.Id bu, PT.Id l, PT.Id v]))

   fun genParamFunDecompress(f,exprs,typedParams, hasFunctionParam) = 
       fn(ps,pd,bu,v,l,cSize) => 
          let val initPs = P.initParams (PT.Id ps,PT.Id pd,typedParams,hasFunctionParam)
	      val callUserDecompX = PT.Call(PT.Id f,
					    [PT.Id ps] @ exprs @ [PT.Id bu, PT.Id l, PT.Id v])
	      val callUserS = P.assignS(PT.Id cSize, callUserDecompX)
	      val state = PT.Compound(initPs @ [callUserS])
	  in
              state
	  end



   fun mapStripeDecompress(name, df, rangeCT,  exprs,
			   typedParams, paramCT, hasFunctionParam) = 
       let val wrapperName = Hid("stripe_dc_wrapper_"^name)
	   val ps = Hid("paramSet")
	   val pd = Hid("paramData")
	   val stripe = Hid("stripe")
	   val sl = Hid("stripeLen")
	   val cdata = Hid ("cdata")
	   val cdataSize = Hid("cdataSize")
           val params = [(P.ctToDT (P.char),    PT.VarDecr ps),
			 (P.ctToDT (P.ptr paramCT), PT.VarDecr pd),
			 (P.ctToDT POR.puint8CT,PT.VarDecr cdata),
			 (P.ctToDT POR.int32CT, PT.VarDecr cdataSize),
			 (P.ctToDT (P.ptr rangeCT) , PT.VarDecr stripe),
			 (P.ctToDT POR.int32CT, PT.VarDecr sl)]
           val initPs = P.initParams (PT.Id ps,PT.Id pd,
				      typedParams,hasFunctionParam)
	   val paramSet = if 0 = (List.length exprs) then [] else [PT.Id ps]
	   val callUserCompX = PT.Call(df,
				       paramSet @ exprs @ [PT.Id cdata, PT.Id cdataSize, 
							   PT.Id stripe, PT.Id sl])
	   val returnS = PT.Return(callUserCompX)
	   val bodyS = PT.Compound(initPs @[returnS])
	   val edecl = P.mkFunctionEDecl(wrapperName,params,bodyS,P.ctToDT POR.int32CT)
       in
	   (wrapperName, edecl)
       end

   (* generate wrapping for decompression function for map name with value
      type ct.  Use funtion genDecompS to generate statements to do actual
      compression.  For defaults, uses genDefaultDecompress(ct).  For functions,
      uses raw function or parameterized function. *)
   fun mapDecompress (name,ct,paramCT,genDecompS) =
       let val fname = Hid ("decompress_"^name)
	   val params = [(P.ctToDT (P.char),    PT.VarDecr "paramSet"),
			 (P.ctToDT (P.ptr paramCT), PT.VarDecr "paramData"),
			 (P.ctToDT POR.puint8CT , PT.VarDecr "bu"),
			 (P.ctToDT POR.int32CT  , PT.VarDecr "l"),
			 (P.ctToDT (P.ptr ct)  , PT.VarDecr "v")]
           val sizDecl = P.varDeclS(P.int, "cSize", P.zero)
	   val defaultS = genDecompS("paramSet", "paramData","bu", "v", "l", "cSize")
	   val checkS = PT.IfThenElse(
			 P.gtX(PT.Id "cSize", PT.Id "l"),
		         HRS.errorS (name^": Consumed too many compressed bytes when unpacking.\n"),
			 PT.IfThenElse(
			  P.eqX(PT.Id "cSize", P.intX ~1), (* HRS_ERROR *)
			  HRS.errorS (name^": Unpacking failed with HRS_ERROR.\n"),
			  PT.IfThenElse(
			   P.eqX(PT.Id "cSize", P.zero),
			   HRS.errorS (name^": Unpack consumed 0 bytes.\n"),
                           PT.IfThen(
			    P.ltX(PT.Id "cSize", P.zero),
			    HRS.errorS(name^": Unpack failed.\n")
                           )
                          )
                         )    
			)
	   val returnS = P.returnS(PT.Id "cSize")
	   val bodyS = PT.Compound [sizDecl, defaultS, checkS, returnS]
       in
	   (fname,P.mkFunctionEDecl(fname,params,bodyS,P.ctToDT POR.int32CT))
       end

   fun tyErr (id, ct) = 
       X.error ("Type of non-parameterized "^id^
		" function ("^
		(CTtoString ct)^
		") does not have a legal type for "^id^".")

   fun chkTy(name, kind, retCT, formalCTs, actCTs) = 
       let val allArgCTs =  [CTchar] @ actCTs
       in
	   if ListPair.all equalType (formalCTs, allArgCTs)
	       andalso (CTisInt retCT) 
	       then ()
	   else (X.error ("Parameterized "^kind^" function "^name^
			  " application is type incorrect.\n"))
       end

    datatype compressType = Generic | UserEntry | UserStripe

    fun doFunCompress (name, rangeType, spec, typedParams,paramCT,hasFunctionParam) = 
	case spec
        of H.NonParam c =>(
	    let val ecompPTct = P.ptr (P.func P.int [P.ptr rangeType,
						    P.ucharPtr,
						    P.int])
		val scompPTct = P.ptr (P.func P.int [P.ptr rangeType, P.int, P.ucharPtr, P.int])
		val (ecompCT,_) = CTcnvType ecompPTct
		val (scompCT,_) = CTcnvType scompPTct
		val _ = pushLocalEnv()
		val (cCT,_) = cnvExp c 
		val _ = popLocalEnv()
		val (typ, cn, edecls) = 
			   if isAssignable(ecompCT, cCT,NONE) then 
			   let val typ = UserEntry 
			       val (cn,cdecl) = mapCompress(name,rangeType,paramCT,
							    genFunCompress c)
			       val edecls = cnvExternalDecl cdecl
			   in
			       (typ, cn, edecls)
			   end
			  else if isAssignable(scompCT,cCT,NONE) then 
			      let val typ = UserStripe
				  val (cn,cdecl) = mapStripeCompress(name,c, rangeType,
								     [], typedParams, paramCT, 
								     hasFunctionParam)
				  val edecls = cnvExternalDecl cdecl
			      in
				  (typ,cn,edecls)
			      end
			  else (tyErr ("compression", cCT); 
				(UserEntry, "Bogus", []))

	    in
		HS.addExtDecls edecls;
		(typ, PT.Id cn)
	    end
	   )
        | H.Param(f,exprs) => 
	   let val _ = pushLocalEnv()
	       val (funCT,_) = cnvExp(PT.Id f)
	       val (typ, cn,edecls) = 
		   (case getFunction funCT
                         of SOME(retCT,formalCTs) => (
			    let val numFormalArgs = List.length formalCTs
				val numActualArgs = List.length exprs
				val actCTs = List.map (#1 o cnvExp) exprs
                            in
				if numFormalArgs = numActualArgs + 4 (* paramSet, bu,v,l *) 
				then let val reqArgsCTs = List.map (#1 o CTcnvType) 
						           [P.ptr rangeType, P.ucharPtr,P.int]
					 val () = chkTy(f, "compression", retCT, formalCTs, 
							(actCTs @ reqArgsCTs))
					 val (cn,cdecl) = mapCompress(name,rangeType, paramCT,
					                     genParamFunCompress(f,exprs,typedParams,
										 hasFunctionParam))
					 val edecls = cnvExternalDecl cdecl
				     in
					(UserEntry, cn, edecls)
				     end
				 else if numFormalArgs = numActualArgs + 5 (* paramSet, stripe,
									      stripeLen, cdata, 
									      cdataExt *)
				 then let val reqArgsCTs = List.map (#1 o CTcnvType)
				                           [P.ptr rangeType,P.int,P.ucharPtr,P.int]
					  val () = chkTy(f, "compression", retCT, formalCTs, 
							 (actCTs @ reqArgsCTs))
					  val (cn,cdecl) = mapStripeCompress(name,PT.Id f,rangeType,
									     exprs, typedParams, paramCT,
									     hasFunctionParam)
					  val edecls = cnvExternalDecl cdecl
                                      in
					  (UserStripe,cn,edecls)
				      end                
				 else (X.error ("Parameterized compression function ("^f^
						   ") passed wrong number of parameters.\n");
				       (UserEntry, "Bogus", []))
			    end
			   (* end SOME *))
                         | NONE => (X.error ("Expected function type for compression function "^f^";"^
					    "found: "^CTtoString funCT);
				     (UserEntry, "Bogus", []))
                         (* end case *))
	       val _ = popLocalEnv()
           in
	      HS.addExtDecls edecls;
              (typ, PT.Id cn)
	   end


    fun doFunDecompress (name,rangeType,spec,typedParams,paramCT,hasFunctionParam) = 
	case spec
        of H.NonParam d =>(
	    let val decompPTct = P.ptr (P.func P.int [P.ucharPtr,
						      P.int,
						      P.ptr rangeType])
		val sdecompPTct = P.ptr (P.func P.int [P.ucharPtr, P.int, P.ptr rangeType, P.int])
		val (decompCT,_) = CTcnvType decompPTct
		val (sdecompCT,_) = CTcnvType sdecompPTct
		val _ = pushLocalEnv()
		val (dCT,_) = cnvExp d
		val _ = popLocalEnv()
		val (typ,dn,edecls) = 
			  if isAssignable(decompCT, dCT,NONE) then 
			      let val typ = UserEntry
				  val (dn,ddecl)= mapDecompress(name,rangeType, paramCT,
								genFunDecompress d)
				  val edecls = cnvExternalDecl ddecl
			      in (typ, dn, edecls) end
			  else if isAssignable(sdecompCT, dCT, NONE) then
			      let val typ = UserStripe
				  val (dn, ddecl) = mapStripeDecompress(name,d, rangeType,
								     [], typedParams, paramCT, 
								     hasFunctionParam)
				  val edecls = cnvExternalDecl ddecl
			      in (typ,dn,edecls) end
			  else (tyErr ("decompression", dCT);
				(UserEntry, "Bogus", []))
	    in
	        HS.addExtDecls edecls;
		(typ, PT.Id dn)
	    end
	   )
        | H.Param(f,exprs) => (
	   let val _ = pushLocalEnv()
	       val (funCT,_) = cnvExp(PT.Id f)
	       val (typ, dn, edecls) = 
		   (case getFunction funCT
                         of SOME(retCT,formalCTs) => (
			    let val numFormalArgs = List.length formalCTs
				val numActualArgs = List.length exprs
				val actCTs = List.map (#1 o cnvExp) exprs
                            in
				if numFormalArgs = numActualArgs + 4 (* paramSet, bu,v,l *)  
				then let val reqArgsCTs = List.map (#1 o CTcnvType) 
						           [P.ucharPtr,P.int,P.ptr rangeType]
					 val () = chkTy(f, "decompression", retCT, formalCTs, 
							(actCTs @ reqArgsCTs))
					 val (dn,ddecl) = mapDecompress(name,rangeType, paramCT,
					                    genParamFunDecompress(f,exprs,typedParams, 
										  hasFunctionParam))
					 val edecls = cnvExternalDecl ddecl
				     in
					(UserEntry, dn, edecls)
				     end
				else if numFormalArgs = numActualArgs + 5 (* paramSet, cdata,
									      cdataSize, stripe, 
									      stripeLen *)
				      then let val reqArgsCTs = List.map (#1 o CTcnvType)
				                           [P.ucharPtr,P.int,P.ptr rangeType,P.int]
					  val () = chkTy(f,"decompression", retCT, formalCTs, 
							 (actCTs @ reqArgsCTs))
					  val (dn,ddecl) = mapStripeDecompress(name,PT.Id f,rangeType,
									     exprs, typedParams, paramCT,
									     hasFunctionParam)
					  val edecls = cnvExternalDecl ddecl
                                      in
					  (UserStripe,dn,edecls)
				      end                

				else (X.error ("Parameterized decompression function ("^f^
						   ") passed wrong number of parameters.\n");
				      (UserEntry, "Bogus", []))
			    end
			   (* end SOME *))
                         | NONE => (X.error ("Expected function type for decompression function "^f^";"^
					    "found: "^CTtoString funCT);
				    (UserEntry, "Bogus", []))
                         (* end case *))
           in
	      HS.addExtDecls edecls;
              (typ, PT.Id dn)
	   end)

    (* Convert a numeric Ctype to a format string *)
    fun CTtoFormat ct : string option =
	case CTgetNum ct of
	    NONE => NONE
	  | SOME(x) =>
		(case x of
		     (Ast.CHAR      ,_           ) => SOME "%c"
		   | (Ast.SHORT     ,Ast.SIGNED  ) => SOME "%hd"
		   | (Ast.SHORT     ,Ast.UNSIGNED) => SOME "%hu"
		   | (Ast.INT       ,Ast.SIGNED  ) => SOME "%d"
		   | (Ast.INT       ,Ast.UNSIGNED) => SOME "%u"
		   | (Ast.LONG      ,Ast.SIGNED  ) => SOME "%ld"
		   | (Ast.LONG      ,Ast.UNSIGNED) => SOME "%lu"
		   | (Ast.LONGLONG  ,Ast.SIGNED  ) => SOME "%lld"
		   | (Ast.LONGLONG  ,Ast.UNSIGNED) => SOME "%llu"
		   | (Ast.FLOAT     ,_           ) => SOME "%f"
		   | (Ast.DOUBLE    ,_           ) => SOME "%lf"
		   | (Ast.LONGDOUBLE,_           ) => NONE)


(* Conversions ***************************************************************)

       fun autoConvert (srcCT,srcAstOpt,targetCT) 
	   : (PT.expression -> PT.expression * (Ast.declaration list)) =
	   let val ropt : HI.RecordInfo option =
		   (case CThinfo srcCT of
			SOME (HI.Record r) => SOME r
		      |_ => NONE)
	       val errMsg = "Type " ^ (CTtoString targetCT) ^
		   " is not compatible with the supplied type ("^
		   (CTtoString srcCT) ^ ")"
	   in
	       if isAssignable(targetCT,srcCT,srcAstOpt) then 
		   (fn x => (x,[]))
	       else case ropt of 
		   NONE => 
		       (X.error errMsg;
			(fn x => (x,[])))
		 | SOME {v1CT,v2CT,toView1,toView2,...} =>
		       if isAssignable(targetCT,v1CT,NONE)
			   then toView1
		       else if isAssignable(targetCT,v2CT,NONE)
				then toView2
			    else X.fail errMsg
	   end   

    (* This function converts a view index into a Hancock map into
       the type the map requires, if one of the views has the right
       type.  It is used in both reading and writing from a map.
       It needs the CT utility functions; hence it is after them. *)

    fun chkIndex(mapX, indexX, onLHS) 
	: {rangeCT:ctype, keyCT:ctype, 
	   keyX:PT.expression, keyDecls: Ast.declaration list} = 
	let val _ = 
	    if topLevel () then
		X.fail "Maps cannot be accessed outside of a function."
	    else ()
	    val (mapCT,mapAst) = cnvExp mapX
	    val _ = if onLHS andalso isConst mapCT then 
		         X.error "Cannot assign to constant maps.\n" else ()
	    val {keyCT, rangeCT, ... } : HI.MapInfo = CTmapInfo mapCT
	    val (indexCT,indexAst) = cnvExp indexX
	    val (keyX,keyDecls) = 
		autoConvert (indexCT,SOME indexAst,keyCT) indexX
	in
	    {rangeCT=rangeCT, keyCT = keyCT, keyX = keyX, keyDecls = keyDecls}
	end

   fun cnvMode mode =
       case mode 
       of H.New => HRS.new
       |  H.Exists => HRS.exists
       |  H.Either => HRS.dontcare

        fun cnvInitH(formalParams, openFunName, openFunNoParamsName, 
		     filename, readonly, mode, params, name) = 
	    let val isParamd = not (0 = List.length formalParams)
		val modeFlag = cnvMode mode
		val readonly = if readonly then HRS.readonly else HRS.readwrite
                val (funCT,_) = cnvExp(PT.Id openFunName)
		val formalArgTys  = 
		    case getFunction funCT
			of SOME(retTy, (argTy1::argTy2::argTy3::argTy4::rest)) => 
			    let val rev = List.rev (argTy2::argTy3::argTy4::rest) 
				(* remove paramSet param *)
				val trunc =  List.tl(List.tl(List.tl(List.tl rev)))
 			        (* remove 4 other req. params *)
			    in 
				List.rev trunc  (* extract type parameters *)
			    end
			
		      | _ => X.bug 
			    "Open Function must be a function of at least 4 arguments.\n"         
                val (funName, paramsSupplied, params) = 
                    case params
                    of [] => if isParamd then
			        let val PTptrTys = List.map (CTtoPTct o Ast.Pointer) formalArgTys
                                     val openName = case openFunNoParamsName
				                of SOME n => n
						| NONE => (X.error ("Types parameterized with "^
								   "function values must be supplied "^
								   "with actual parameters at initialization.\n");
							   openFunName)
				in
				    (openName, P.falseX, [])
				end
			      else (openFunName, P.trueX,[]) (* all necessary params (none) supplied.
							        Tested in nested case. *)
                    | _ => let 
			       val _ = if (List.length formalArgTys = List.length params)  then ()
				       else X.error ("Wrong number of parameters supplied in initializing" ^
						     " declaration for "^ name^ ".\n")
(*  Earlier variables in declaration block are not in scope, and so
    this error checking produces spurious errors.  Omit for now. KSF *)
(*			       val actualArgs = List.map cnvExp params
			       val zipped = ListPair.map (fn (fTy,(aTy,aExp))=> (fTy,aTy,SOME aExp))
				                         (formalArgTys, actualArgs)
			       val _ = if List.all isAssignable zipped
					   then ()
				       else X.error ("Parameter supplied in initializing" ^
						     " declaration for "^ name^ " has the wrong type.\n") *)
			   in
			       (openFunName, P.trueX, params)
			   end
	    in
		PT.Call(PT.Id funName,
			[paramsSupplied] @ params @ [filename, modeFlag, readonly, P.trueX])
	    end


(* This function is here because they are needed to convert sigmain *)
    fun cnvMapOpen (var        :string,
		    name       :PT.expression,
		    mode       :H.Hmode, 
		    readonly   :bool,
		    mi         : HI.MapInfo,
		    tyParams: PT.expression list) : PT.statement =
	let val _ = 
		case (mode,readonly) of
		    (H.New, true) => X.error "Opening new constant map."
		  | _ => ()
	    val openX = cnvInitH(#formalParams mi, #initFun mi, #initFunNoParams mi, 
				 name, readonly,mode,tyParams, var)
	    val openS = P.assignS(P.alval(HRS.mapCT,HRS.mapAccessX (PT.Id var)), openX)
	in
	    openS
	end

(* This function is here because they are needed to convert sigmain *)
    fun cnvPickleOpen (var        :string, 
		       name       :PT.expression,
		       mode       :H.Hmode, 
		       readonly   :bool,
		       pi          :HI.PickleInfo,
		       tyParams   :PT.expression list) : PT.statement =
	let val _ = 
		case (mode,readonly) of
		    (H.New, true) => X.error "Opening new constant pickle."
		  | _ => ()
	    val openX = cnvInitH(#formalParams pi, #initFun pi, #initFunNoParams pi, 
				 name, readonly, mode, tyParams, var)

	    val openS = P.assignS(P.alval(#typ pi, PT.Id var), openX)
	in
	    openS
	end

(* This function is here because they are needed to convert sigmain *)
    fun cnvStreamOpen (var        :string, 
		       name       :PT.expression,
		       mode       :H.Hmode, 
		       readonly   :bool,
		       si         :HI.StreamInfo,
		       tyParams   :PT.expression list) : PT.statement =
	let val _ = 
		case (mode) of
		    H.New => X.error "Streams must exist.\n"
		  | _ => ()
	    val openX = cnvInitH(#formalParams si, #initFun si, #initFunNoParams si, 
				 name, readonly, mode, tyParams, var)

	    val openS = P.assignS(P.alval(#typ si, PT.Id var), openX)
	in
	    openS
	end


    fun cnvDirOpen(var:string, name:PT.expression, mode:H.Hmode, readonly:bool,
		   di : HI.DirInfo, tyParams :PT.expression list) : PT.statement   =
	       let val _ =
		   case (mode,readonly) 
		     of (H.New, true) => X.error "Opening new constant directory."
		     | _ => ()
		   val openX = cnvInitH(#formalParams di, #initFun di, #initFunNoParams di, 
					name, readonly, mode, tyParams,var)
		   val openS = P.assignS(P.alval(#PTtypeCT  di, PT.Id var), openX)
	       in
		   openS
	       end
		
(* Conversions : sig_main ****************************************************)

   type HParam = {param  : string, (* name of the parameter *)
		  switch : string, (* switch associated with it *)
		  msg    : string option, (* help message if available *)
		  hasArg : bool, (* does this switch take an argument? *)
		  decl   : PT.statement, (* Declaration for the parameter *)
		  convert: PT.statement, (* Code to run on optarg when this
					    switch seen *)
		  default: PT.statement} (* Code to run add the end of the
					    day if this switch was not seen
					    at all. *)

   fun genUsageString long (params : HParam list) =
       let fun usageParam ({switch,hasArg,...} : HParam) =
	   "-" ^ switch ^ (if hasArg then ":" else "") ^ " "

	   fun longUsageParam ({switch,hasArg,msg,...} : HParam) =
	       let val hd = "\t-"^switch ^(if hasArg then ":" else " ") ^"\t"
	       in
		   case msg of 
		       NONE => hd^"\n"
		     | SOME m => hd ^ m ^ "\n"
	       end
       in
	   if long then "usage: \n" ^ (HL.smap longUsageParam params)
	   else "usage: " ^ (HL.smap usageParam params) ^ "\n"
       end

   fun genSwitchStruct (params : HParam list) = 
       let fun oneSwitch({switch,hasArg,...} : HParam) = 
	       (P.charPtr,[(PT.VarDecr switch, PT.EmptyExpr)])
	   val structMembers = List.map oneSwitch params
           val switchStructName = Hid ("switchStruct")
           val switchRepPT = P.makeCT[PT.Struct{ isStruct = true,
		 	          tagOpt = SOME (switchStructName ^"_s"),
				 members = structMembers }]
           val switchPT = P.typedef switchStructName
           val (switchCT,_) = CTcnvType switchRepPT
           val switchTid = ASTtypedef(switchStructName, switchCT)
           val ckitDecls = List.map declTid (resetTids())
           val switchDecls = ckitDecls @ [declTid switchTid]
       in
	   (switchPT, switchDecls)
       end

   fun genGetFlags (onCommand : string) (params : HParam list) : PT.statement * Ast.externalDecl list =
       let val numParams = List.length params

	   val seen = Hid "seen"
	   val seenDecl = P.varDeclS(P.array(P.intX numParams, P.char),
				     seen,
				     PT.InitList [P.zero])
           val (switchPT, switchDecls) = genSwitchStruct params

           val switchDecl = P.varDeclS(switchPT, onCommand, PT.InitList [P.zero])

	   val flag = Hid "flag"
	   val flagDecl = P.varDeclS(P.int,flag,P.zero)

	   fun switchCode (i:int) (p : HParam) =
	       let val {param,switch,hasArg, ...} = p
		   val caseNum = Char.ord (String.sub (switch,0))
		       
		   val error1 = HRS.errorS ("Switch -"^switch^
					  " requires an argument.\n")
		   val error2 = HRS.errorS ("Switch -"^switch^
					  " cannot take an argument.\n")

		   val error3 = HRS.errorS ("Switch -"^switch^
					  " can only occur once.\n")

		   val alreadySeenS =
		       PT.IfThen(P.subX(PT.Id seen, P.intX i),
				 error3)

                   val assignSwitchS = checkString 
		                       (P.dotX(PT.Id onCommand, PT.Id switch)) 
				       (CL.optarg) 

		   val checkS = 
		       if hasArg then 
			   PT.IfThenElse(P.eqX(CL.optarg,P.zero), error1, assignSwitchS)
		       else PT.IfThen(P.neqX(CL.optarg,P.zero), error2)


		   val bodyS = 
		       P.compoundS [
				    alreadySeenS,
				    checkS,
				    P.assignS(P.subX(PT.Id seen, P.intX i),
					      P.intX 1),
				    PT.Break]
	       in
		   PT.CaseLabel(P.intX caseNum,
				bodyS)
	       end

	   val usageString = genUsageString false params
	   val longUsageString = genUsageString true params

	   (* getopts returns a colon character if it detects a missing argument. *)
 
	   val missingArgS = PT.CaseLabel(P.intX (Char.ord #":"),
			     HRS.errorS2 ("Switch -%c requires an argument.\n", 
					[CL.optopt],
					usageString))
	   val defaultS = PT.DefaultLabel (
			     HRS.errorS2 ("Illegal option: %c.\n", 
					[CL.optopt],
					usageString))

	   val mainLoopS = 
	       let fun loop i [] = [missingArgS, defaultS]
		     | loop i (hd::tl) = (switchCode i hd)::(loop (i+1) tl)
		   val switchS = PT.Switch(PT.Id flag,
					   PT.Compound (loop 0 params)) 
		   fun optString ({switch,hasArg,...}:HParam) = 

		       switch ^ (if hasArg then ":" else "")
		   val getoptString = ":" ^ HL.smap optString params (* tell getopts to
									report missing args.*)

		   val testX = P.neqX (P.assignX (PT.Id flag,
						  CL.getopt(CL.argc,
							   CL.argv,
							   PT.String getoptString)),
				       PT.Unop(PT.Negate,PT.IntConst (IntInf.fromInt 1)))
	       in
		   PT.While(testX, switchS)		   
	       end

	   
           val chkUsageQueryS = 
	       PT.IfThen(P.eqX(CL.strcmp(P.subX(CL.argv,
					       P.minusX(CL.optind, P.intX 1)),
					PT.String "--"),
			       P.zero),
			 HRS.errorS(longUsageString))

	   val doDefaultS =
	       let fun doOne (i:int) ({convert,default,switch,hasArg,...}:HParam) = 
		   let val deallocSs = if hasArg 
				       then [PT.Expr(CL.free(P.dotX(PT.Id onCommand, 
							    PT.Id switch)))]
				       else []
		   in
		     PT.IfThenElse(P.notX(P.subX(PT.Id seen,P.intX i)),
			           default, 
				   PT.Compound ( convert ::deallocSs))
		   end
		   fun loop i [] = []
		     | loop i (hd::tl) = (doOne i hd) :: (loop (i+1) tl)
	       in
		   P.compoundS (loop 0 params)
	       end


       in
	   (P.compoundS [ P.commentS "Start of getFlags",
			 seenDecl,
			 switchDecl,
			 flagDecl,
			 P.assignS(CL.opterr, P.zero),
			 mainLoopS, (* read command-line arguments *)
			 chkUsageQueryS,
			 doDefaultS, (* process defaults for undetected *)
			 P.commentS "End of getFlags"
			],
	    switchDecls)

       end


   fun buildMain body hRetTy=  
      let val cRetTy = P.ctToDT hRetTy
      in
	  (* Cannot use P.funDef because main must not be static. *)
        PT.FunctionDef {body    = P.compoundS body,
			funDecr = PT.FuncDecr (PT.VarDecr "main",
					      [(P.ctToDT P.int,
						PT.VarDecr "argc"),
					       (P.ctToDT P.constCharPtr, 
						PT.ArrayDecr (PT.VarDecr "argv",PT.EmptyExpr))]),
			krParams= [],
			retType = cRetTy}
      end

   fun overflowS paramName typ = HRS.errorS ("Command-line argument passed to parameter "^
				   paramName^" doesn't fit type "^
				   CTtoString(typ)^".\n")

   (* check??? functions return code that dynamically checks the validity
      of the string returned by optarg for the appropriate type. *)


   fun overflowTestS paramName typ = 
       PT.IfThen(P.eqX(CL.errno,CL.ERANGE), overflowS paramName typ) 

   fun formatS gotTo paramName typ =
       PT.IfThen(P.neqX(CL.strlen(PT.Id gotTo),P.zero),
		 HRS.errorS ("Non-numeric command-line argument " ^
			   "passed to parameter "^
			   paramName^
			   " of type "^
			   CTtoString(typ)^".\n"))

   fun checkDoubles PTty cFun paramName argX typ = 
       let val gotTo =  Hid (paramName^"gotTo")
       in
	   P.compoundS [P.varDeclS(P.charPtr, gotTo, P.zero),
			P.assignS(
				  P.alval(PTty, PT.Id paramName),
				  cFun(argX,
				       P.addrX(PT.Id gotTo))),
			formatS gotTo paramName typ,
			overflowTestS paramName typ]
       end

   fun checkFloat paramName argX typ = 
       let val gotTo =  Hid(paramName^"gotTo")
	   val doubleversion =  Hid(paramName^"doubleversion")
       in
	   P.compoundS [
			P.varDeclS(P.charPtr, gotTo, P.zero),
			P.varDeclS(P.double, doubleversion, P.zero),
			P.assignS(PT.Id doubleversion,
				  HRS.strtod(argX,
					   P.addrX (PT.Id gotTo))),
			formatS gotTo paramName typ,
			overflowTestS paramName typ,
			P.assignS(
				  P.alval(P.float, PT.Id paramName),
				  PT.Cast(P.float, PT.Id doubleversion))]
       end



   fun checkInts PTty PTmax PTmin paramName argX typ = 
       let val gotTo =  Hid(paramName^"gotTo")
	   val longversion =  Hid(paramName^"longversion")
       in
            P.compoundS [
               P.varDeclS(P.charPtr, gotTo, P.zero),
               P.varDeclS(P.longlong, longversion, P.zero),
               P.assignS(PT.Id longversion,
                         HRS.strtoll(argX,
                                  P.addrX (PT.Id gotTo),
				  P.intX 10)),
	       formatS gotTo paramName typ,
	       overflowTestS paramName typ,
               PT.IfThen(P.gtX(
                           PT.Id longversion,
                           PTmax),
			 overflowS paramName typ),
               PT.IfThen(P.ltX(
                           PT.Id longversion,
                           PTmin),
			 overflowS paramName typ),
               P.assignS(
                 P.alval(PTty, PT.Id paramName),
                 PT.Cast(PTty, PT.Id longversion))]
       end



   fun checkLongs PTty cfun signed paramName argX typ =
       let val gotTo =  Hid (paramName^"gotTo")
	   val longversion = Hid(paramName^"longversion")
	   val minCheckS = if signed then []
			   else [PT.IfThen(P.notX(HRS.isPositive (argX)),
					   overflowS paramName typ)]
       in
	   P.compoundS ([P.varDeclS(P.charPtr, gotTo, P.zero)] @
			minCheckS @
			[P.assignS(P.alval(PTty, PT.Id paramName),
				   cfun(argX,
					P.addrX(PT.Id gotTo),
					P.intX 10)),
			 formatS gotTo paramName typ,
			 overflowTestS paramName typ])
       end


   (* Case when char has argument !! *)
   fun checkChar paramName argX typ = 
       let val (PTmin, PTmax, PTchar) = 
	   if CTisSChar typ then (P.charMin, P.charMax, P.char)
	   else (P.zero, P.ucharMax, P.uchar)

	   val longversion =  Hid(paramName^"longversion")
	   val noerror =  Hid(paramName^"noerror")
	   val checkIntS = checkInts PTchar PTmax PTmin paramName argX typ

	   val readS = P.assignS(PT.Id noerror, 
				 (HRS.getChar (argX) 
				            (P.addrX(PT.Id longversion))))
	   val ltMaxX = P.gtX(PT.Id longversion, PTmax)
	   val gtMinX = P.ltX(PT.Id longversion, PTmin)
	       
	   val lval = P.alval(PTchar, PT.Id paramName)
	   val rval = PT.Cast(PTchar, PT.Id longversion)

       in 
	   P.compoundS [P.varDeclS(P.longlong, longversion, P.zero),
			P.varDeclS(P.int, noerror, P.zero),
			readS,
			PT.IfThenElse(PT.Id noerror,
				      P.compoundS[
						  PT.IfThen(ltMaxX,
							    overflowS paramName typ),
						  PT.IfThen(gtMinX,
							    overflowS paramName typ),
						  P.assignS(lval, rval)
						  ],
				      checkIntS)
			]
       end

   fun checkArg hasArg cty name = if not hasArg 
                   then ignore(X.error (
                                "Sig_main parameters of type "^
                                (CTtoString cty)^  " (" ^ name ^")"^
                                " must take a command-line argument."))
                   else ()

   fun chkCnvMode (mode, const, name) =  
       if mode = H.New then
	   if const then
	       X.fail ("Sig_main parameter (" ^ name ^")"^
		     " cannot be both const and new.")
	   else HRS.new
       else cnvMode mode

   fun cnvPickleParam (pi : HI.PickleInfo) cty mode arg name tyPs= 
       let val readonly = isConst cty
	   val pickleInitS = cnvPickleOpen(name, arg, mode, readonly, pi, tyPs)
       in
	   pickleInitS
       end

   fun cnvStreamParam (si : HI.StreamInfo) cty mode arg name tyPs =
       let val readonly = isConst cty
           val streamInitS = cnvStreamOpen(name,arg,mode,readonly,si,tyPs)
       in
	   streamInitS
       end
(*
   fun cnvStreamParam (s : HI.StreamInfo) arg name =
       let val {PTlogicalCT,PTphysicalCT,isIO,...} = s
           val logicalSize = P.sizeofX PTlogicalCT
           val physicalSize = if isIO then P.zero else P.sizeofX PTphysicalCT
       in
	   P.assignS (P.alval(HRS.streamCT, PT.Id name),
		      HRS.streamReg (arg,
				    physicalSize,
				    logicalSize))  
       end
*)


   fun cnvMapParam (mi : HI.MapInfo) cty mode arg name tyPs =
       let val readonly = isConst cty
	   val mapOpenS = cnvMapOpen (name, arg,mode, readonly, mi, tyPs)
       in
	   mapOpenS
       end


   fun cnvDirParam (d : HI.DirInfo) cty mode arg name tyPs =
       let val readonly = isConst cty
           val dirInitS = cnvDirOpen(name,arg,mode,readonly,d,tyPs)
       in
	   dirInitS
       end

   fun getHTyInfo ct = 
       let val hinfo = CThinfo ct
           val (baseTy, tyParams) = case hinfo
               of SOME (HI.Param{basety,exprs}) => (basety, exprs)
               | _ => (ct, [])
       in
	   (CThinfo baseTy, tyParams)
       end

   fun genHconvert cty mode arg param =
	   case getHTyInfo cty of
	       (SOME(HI.Map m), tyP)    => cnvMapParam    m cty mode arg param tyP
	     | (SOME(HI.Pickle p), tyP) => cnvPickleParam p cty mode arg param tyP
	     | (SOME(HI.Dir s), tyP)    => cnvDirParam    s cty mode arg param tyP
	     | (SOME(HI.Stream s), tyP) => cnvStreamParam s cty mode arg param tyP
	     | _ => (X.error ("Hancock cannot process command-line "^
			   "arguments of type: "^CTtoString(cty)^"\n");
		     P.emptyS)

   (* XXX - this function is probably not factored perfectly. 
    No actually it just sucks!!! *)
   fun genDefault param switch ptct cty default hasArg mode =
       let val hinfo = CThinfo cty
	   val stringOk = 
	       case hinfo of
		   (SOME(HI.Map _) | 
		    SOME(HI.Dir _) |
		    SOME(HI.Stream _) |
		    SOME(HI.Pickle _) |
                    SOME(HI.Param _)) => true
		  | _ => false
	   val argstr = if hasArg then ":" else ""
	   val errorS =  HRS.errorS ("Must invoke with -"^switch^
				   argstr^" switch \n")

	   val initValueX = case hinfo 
	                    of SOME(HI.Stream s) => PT.EmptyExpr (* not streams 
								because of generative 
								streams. *)
                            | _ =>  P.zero (* initialize everything to zero *)

	   fun hDefaultS def = 
	       case hinfo of
		   SOME hi => genHconvert cty mode def param 
		 | _ => X.bug "Unexpected case."


	   val defaultS =
	       (case default of 
		    NONE => if hasArg then errorS else P.emptyS
		  | SOME exp => 
			let val (expTy,expAst) = cnvExp exp
			    val ok = isAssignable(cty,expTy,SOME expAst)
			in
			    if ok then 
				if (CTisString cty)
				then 
				    PT.Compound[PT.IfThen(exp, checkString (PT.Id param) exp)]
				else
				P.assignS(P.alval (P.removeConst ptct, PT.Id param), exp)  
			    else if (stringOk andalso (CTisString expTy)) then
				hDefaultS exp
				 else (X.error ("Type of default (" ^
					      (CTtoString expTy) ^
					      ") does not match type of " ^
					      param ^
					      ".");
				       errorS)
			end)
       in
	   (defaultS,initValueX)
       end

   fun genConvert param argX mode cty =
       let val hinfo = CThinfo cty
	   val _ =
	       (case hinfo of
		    NONE => if mode <> H.Either then
			(X.error "Cannot specify new or exists for C types.")
			    else ()
		  | _ => ())
	   fun err () =  X.error ("Hancock does not support command-line "^
				"arguments of type: "^CTtoString(cty)^"\n")
	   
	   val sshort = (P.short , P.shortMax  , P.shortMin)
	   val ushort = (P.ushort, P.ushortMax , P.zero)
	   val   sint = (P.int   , P.intMax    , P.intMin)
	   val   uint = (P.uint  , P.uintMax  , P.zero)
	       
	   val      long = (P.long     , HRS.strtol  , true)
	   val     ulong = (P.ulong    , HRS.strtoul , false)
	   val slonglong = (P.longlong , HRS.strtoll , true)
	   val ulonglong = (P.ulonglong, HRS.strtoull, false)

	   val double = (P.double, HRS.strtod)
(*	   val longdouble = (P.longdouble, HRS.strtold) *)

	   fun chInt (t,max,min) = checkInts t max min param argX cty
	   fun chLong (t,func,bools) = checkLongs t func bools param argX cty
	   fun chDouble (t,func) = checkDoubles t func param argX cty
       in 
	   case hinfo of
	       SOME hi => genHconvert cty mode argX param
	     | NONE => 	   
		   (if      CTisString     cty then checkString (PT.Id param) argX 
		    else if CTisChar       cty then checkChar param argX cty
	            else if CTisSShort     cty then chInt sshort
	            else if CTisUShort     cty then chInt ushort
	            else if CTisSInt       cty then chInt sint
	            else if CTisUInt       cty then chInt uint
	            else if CTisSLong      cty then chLong long
	            else if CTisSLongLong  cty then chLong slonglong 
	            else if CTisULong      cty then chLong ulong
	            else if CTisULongLong  cty then chLong ulonglong
	            else if CTisFloat      cty then checkFloat param argX cty
	            else if CTisDouble     cty then chDouble double
(*	            else if CTisLongDouble cty then chDouble longdouble *)
	            else if CTisLongDouble cty then (err(); P.emptyS)
		    else (err (); P.emptyS))
       end

   fun cnvParam onCommand (p:HSigParam) = 
       let val {errorMsg, name, typ,default,switch,hasArg,msg} = p
	   val _ = ifParseError errorMsg
   
	   val switch = 
	       if not (String.size switch = 1) then 
		   (X.error ("Switches must be one character long. "^
			   "Switch "^switch^" is too long.");
		    String.substring(switch,0,1))
               else switch

	   val (ct,decr) = typ
	   val (mode,ct') = P.extractModeCT ct

	   val (cty, nameOpt) = CTcnvDecr (ct',decr)
           val pname = 
	       case nameOpt
		   of NONE => (X.error "Parameter must be named.\n"; "bogus")
		 | SOME(name) => name 
	   val (defaultS,initialX) = 
	       genDefault pname switch ct' cty default hasArg mode
	   val initialD = P.ctDeclS(ct', decr, initialX)
	   val _ = localInitVar(pname,cty)

	   val _ = 
	       if not (CTisChar cty) andalso not hasArg then
		   ignore(X.error (
				 "Sig_main parameters of type "^
				 (CTtoString cty)^  " (" ^ pname ^")"^
				 " must take a command-line argument."))
	       else ()

	   val convertS = 
	       if hasArg then 
                  genConvert pname (P.dotX(PT.Id onCommand, PT.Id switch)) mode cty
	       else let val lval = P.alval(P.char, PT.Id pname)
		    in case default of 
			 NONE => P.assignS(lval, P.intX 1)
		       | SOME e => P.assignS(lval, P.notX e)
		    end
       in
	   { param = pname,
	     switch = switch,
	     msg = msg,
	     hasArg = hasArg,
	     decl = initialD,
	     convert = convertS,
	     default = defaultS
	     } : HParam
       end

   fun cnvFirst (rawFirst:HSigParam) =
       let val _ = ifParseError (#errorMsg rawFirst)
	   val (ct,decr) = #typ rawFirst
	   val (mode,ct') = P.extractModeCT ct
	   val (cty, nameOpt) = CTcnvDecr(ct', decr)
	   val name = (case nameOpt of 
			   NONE => (X.error "Program name parameter must have a name.";
				    "Error_unspecified_program_name")
			 | SOME name => name)
	   val _ = if CTisString cty then () else 
	       X.error "Program name parameter must have type char *."
	   val _ = 
	       (case mode of 
		    H.Either => ()
		  | _ => X.error "Cannot specify new or exists for program name parameter.")
	   val _ = 
	       case #default rawFirst of 
		   NONE => ()
		 | _ => X.error "Cannot specify default value for program name parameter."
           val decll = [P.ctDeclS(ct', decr, P.zero)]
	   val initSs = [P.assignS(PT.Id name,
				   CL.malloc(P.plusX(CL.strlen(HRS.programNameId), 
						    P.intX 1))),
			 PT.Expr(CL.strcpy (PT.Id name, HRS.programNameId))]

       in
	   (decll, initSs)
       end

   fun cnvParams arg params = 
       let val _ = pushLocalEnv ()
	   val result = map (cnvParam arg) params
	   val _ = popLocalEnv ()
       in
	   result
       end

   fun checkMultipleSigMain () = 
       if !seenSigMain 
       then X.bug "Program contains multiple sig_main functions.\n"
       else seenSigMain := true

   fun cnvSigMain {retType = retTy, params = params, body = body} = 
        let val _ = checkMultipleSigMain ()
	    val (params,fdecll,fInitSs) = 
		(case params of
		     [] => ([],[],[])
		   | (ps as hd::tl) => 
			 (if (#switch hd) <> "" then (ps, [], [])
			  else let val (d,i) = cnvFirst hd
			       in
				   (tl,d,i)
			       end))

	    val onCommand = Hid "onCommand"
            val paramCode = cnvParams onCommand params

	    fun paramEq (param1 : HParam) (param2 : HParam) =
		let val {switch=s1,param=p1,...} = param1
		    val {switch=s2,param=p2,...} = param2
		in
		    (s1=s2) orelse (p1=p2)
		end

	    fun duplicate [] = false
	      | duplicate (hd::tl) = ((List.exists (paramEq hd) tl) 
				      orelse duplicate tl)

	    val _ = 
		if duplicate paramCode then 
		    X.error "Sig_main had duplicate parameters or switches." 
		else ()

	    val (getFlagsS, switchDecls) = 
		case paramCode of 
		    [] =>  (P.emptyS,[])
		  | _ => genGetFlags onCommand paramCode

	    val paramDecls = 
		List.map (fn ({decl,...}:HParam) => decl) paramCode
		     
            val main = buildMain (paramDecls @
				  fdecll @
				  [HRS.init(P.subX(PT.Id "argv", P.zero))]  @ 
				  fInitSs @
				  [ getFlagsS ] @
				  [ body ] @
				  [ P.returnS P.zero]) P.int
            val mainAST = (
		pushLocalEnv();
		ignore(insTempVar("errno", P.int));
		(cnvExternalDecl main) before 
		  popLocalEnv())
        in
	   switchDecls @ mainAST
(*           (cnvExternalDecl main) (* Ast.externalDecl list *) *)
        end

(* Conversions : Iterate *****************************************************)
   (* also used in cnvWindow in declaration section below. *)

   datatype streamKind = 
       Map of (HI.MapInfo * 
	       PT.expression option * PT.expression option) (* low and high range, if given *)
     | Stream

    (* Convert an iterate statement
     Hoisted to the top level because I expect it to be complicated. *)
   fun cnvIterate ({errorMsg, over = streamExp, range, 
		    filter, sort, detector, events}: HIterates) =
       let val _ = ifParseError errorMsg  (* check iterates error cond. *)
	   (* Each stream has a physical (on-disk) representation and
	    a logical (in-memory) representation.  In addition
	    depending on whether the stream is from a map or an actual
	    stream we need different kinds of information.  

	    We construct an expression transformX that transforms the
            physical to the logical representation.  (This expression
            is always a function pointer.)  *)

	    (* The first thing we do is compute:
	       the logical and physical types,
	       the kind of stream (from a map or regular),
	       and the transforming expression.*)


	   val (PTlogCT,PTphysCT,streamInfo, transformX, isGenerative,
		winitPs, argPs, paramTy, defEvent) =
	       let val (ct,_) = cnvExp streamExp
		   val ct' = case CThinfo ct
		             of SOME(HI.Param{basety,exprs}) => basety
                             | _ => ct
	       in
		   case CThinfo ct' of
		       SOME (HI.Stream s) =>
			   let fun die () = 
			       X.fail "Ranges disallowed for stream iteration"
			       val _ = 
				   case range of 
				       (NONE,NONE) => () 
				     | _ => die ()
			       val winitPs = P.initParams(PT.Id "paramSet", PT.Id "paramData",
							 #formalParams s, false)
			       val args = #transformArgs s
			       val args = if List.length args = 0 then [] else (PT.Id "paramSet") :: args
			   in
			       (#PTlogicalCT s, #PTphysicalCT s, 
				Stream,
				PT.Id (#transform s), #isGenerative s, winitPs, args, 
				#paramTy s,  #defEvent s)
			   end 
		     | SOME (HI.Map mi) =>
			   let val keyCT = #keyCT mi
			       fun convert e =
				   let val (ct,ast) = cnvExp e
				       val (x',d') = 
					   autoConvert(ct,SOME ast,keyCT) e
				       val ct = case d' 
					        of [] => if (CTisLongLong keyCT) (*long long case*)
							     then keyCT
							 else ct
                                                | _ => ct
				   in
				       (ct,x',d')
				   end
			       val (lowX,highX,rCT,rDecls,coerce) =
				   case range of 
				       (SOME e1, SOME e2) => 
					   let val (lowCT,lowX,lowDecls) = convert e1
					       val (hiCT,hiX,hiDecls) = convert e2
					       val _ = 
						   if isAssignable(lowCT,hiCT,NONE) 
						       then ()
						   else X.error("Both range exnpressions "^
								"must have the same type.\n")
					       val coerce = autoConvert(keyCT,NONE,lowCT)

					   in
					       (SOME lowX, SOME hiX,
						lowCT, lowDecls@hiDecls,coerce)
					   end

				     | _ => 
                                        if CTisLongLong (#keyCT mi) 
					    then 
					       (NONE, NONE,
						#keyCT mi, 
						[], (fn x=>(x,[])))
					else
					    X.fail ("Hancock 1.1 (and earlier) style map iteration requires an "^
						  "upper and a lower bound.\n")


			       val PTphysCT = #PTkeyCT mi
			       val PTlogCT = CTtoPTct rCT

			       val physCT = #keyCT mi
			       val logCT = rCT

			       val transformX =
				   let val transformName = Hid "transform"
				       val transformId = PT.Id transformName
				       val physName =  "pr"
				       val physId = PT.Id physName
				       val logName =  "lr"		       

				       val logId = PT.Id logName 
				       val (keyX, keyDecls) = 
					   coerce (P.starX(physId))
				       val bodyS = P.compoundS
					   [P.assignS(P.starX(logId), keyX),
					    P.returnS (P.intX 1)]
				       val externalDecl = 
					   P.funDef(transformName,
						    [(physName,P.ptr PTphysCT),
						     (logName, P.ptr PTlogCT)],
						    P.int,
						    bodyS)
				       val transformCT = 
					   Ast.Function(CTint, 
							[Ast.Pointer physCT,
							 Ast.Pointer logCT])
				   in
				       (* Put transform in scope *)
				       bindGlobal(transformName, transformCT); 
				       (* Float the external decl *)
				       HS.PTaddExtDecls [externalDecl];
				       HS.addDecls keyDecls;
				       transformId
				   end
			   in
			       HS.addDecls rDecls; (* Float these up. *)
			       (PTlogCT,
				PTphysCT,
				Map (mi,lowX,highX),
				transformX,false,[],[], P.void, 
				#defEvent mi)
			   end
		     | _ => (X.error ("Expected expression with map or stream type " ^
				      "in over clause.\n");
			     (P.int, P.int, 
				Stream,
				PT.Id ("bogus"),false,[],[],P.void, "Bogus"))
	       end

	   val (physCT,_) = CTcnvType PTphysCT
	   val (logCT,_) = CTcnvType PTlogCT

	   (* The four variables PTphysCT, physCT, PTlogCT, logCT are used
	    pervasively below to refer to the physical and logical type of the
	    stream. *)

	   (* type check filteredby subclause 
              transform expression filteredby's to function calls
	      Needed by wrapperX *)
	   val filter = 
	       let fun err1 ct = X.fail("Filter function had non-function type "^
				      (CTtoString ct)^".")
		   fun err2 ct = X.fail("Argument to filter should have type "^
				      CTtoString ct)
		   fun err3 () = X.fail "Filter function takes one argument."
		   fun err4 () = X.fail "Filter functions must return an int."
	       in
		   case filter of
		       NONE => NONE
		     | SOME (H.FilterFun f) => 
			   let val (ct,ast) = cnvExp (PT.Id f)
			       val (retCT,argCTs) = 
				   case getFunction ct of
				       SOME (r,args) => (r,args)
				     | _ => err1 ct
			       val expectedArgCT = Ast.Pointer logCT
			       val _ = 
				   case argCTs of
				       [r] => if isAssignable(expectedArgCT,r,
							      NONE) 
						  then () 
					      else err2 expectedArgCT
				     | _ => err3 ()
			       val _ = if (CTisInt retCT) then ()
				       else err4 ()
			   in
			       SOME (fn arg =>PT.Call(PT.Id f,[arg]))
			   end		   
                     | SOME (H.FilterExpr(s,exp)) => 
			   SOME(fn arg => HPTS.substExp(s,arg,exp))
                      
	       end

	   (* The wrapper function applies the transformation
	   expression and the filtered by clause.  The return
	   expression is just a pointer to the right function.  *)

	   val wrapperX = 
	       let val wrapperName = Hid "wrapper"
		   val wrapperId = PT.Id wrapperName
		   val goodFlag =  "isGood"
		   val goodFlagId = PT.Id goodFlag
		   val wantedFlag =  "isWanted"
		   val wantedFlagId = PT.Id wantedFlag
		   val physName =  "pr"
		   val physArgs = if isGenerative then [] else [PT.Id physName]
                   val physParams = if isGenerative then [] else [(physName, P.ptr PTphysCT)]
                   val physCTs  = if isGenerative then [] else [Ast.Pointer physCT]
		   val logName =  "lr"			
		   val logId = PT.Id logName 
		       
		   val wAssignSs = 
                       case filter of NONE => []
                          | SOME f => [P.assignS(wantedFlagId, f logId)]
		   fun testS s =
		       PT.IfThen(goodFlagId,
				 P.compoundS(wAssignSs @
					     [PT.IfThen(wantedFlagId,s)]))
		   val bodyS = 
		       P.compoundS
		       (winitPs @
			[P.varDeclS (P.int,goodFlag,PT.EmptyExpr),
			P.varDeclS (P.int,wantedFlag,P.trueX),
			P.assignS(goodFlagId,
				  PT.Call(transformX,
					  argPs @ physArgs @[logId])),
(*			PT.IfThenElse(P.lteX(goodFlagId,P.zero), 
				      PT.Return goodFlagId,
				      P.compoundS(wAssignSs)),
			PT.Return wantedFlagId*)
			PT.IfThenElse(P.eqX(goodFlagId, HRS.streamKeep),
				      P.compoundS(wAssignSs @ [PT.Return wantedFlagId]),
				      PT.Return goodFlagId)
			 ])

		       val externalDecl = 
			   P.funDef(wrapperName,
				    [("paramSet", P.char),
				     ("paramData", P.ptr paramTy)]
				    @ physParams @
				     [(logName, P.ptr PTlogCT)],
				    P.int,
				    bodyS)

		       val externalCT = Ast.Function(CTint,
						     [CTchar,
						      Ast.Pointer (#1 (CTcnvType paramTy))]
						      @ physCTs @ 
						      [Ast.Pointer logCT])
	       in
		   bindGlobal(wrapperName,externalCT);
		   HS.PTaddExtDecls [externalDecl];
		   wrapperId
	       end

	   (* SORTING *) (* right now, key specifications are just
	   strings. *) (* given a logical stream record (parse tree
	   and ast reps) and a key specification, return the type, the
	   offset for that key in the record, and the collection of
	   atomic keys covered by the key spec. *)
	   fun findKey(PTlogicalCT : PT.ctype, logicalCT : ctype) 
	       (keySpec : string) 
	       : (ctype * PT.expression * string )
	       =
	       let 
		   val (_, members) = CTstructMembers "2" logicalCT
		   fun membEq s (_,{name,...}:Ast.member) = 
		       ((SYM.name name) = s)
	       in
		   (case List.find (membEq keySpec) members
			of NONE => 
			    (X.error ("Sorting specification "^
				    keySpec^
				    " is not a field of logical stream type: "^
				    (CTtoString logicalCT)^".\n");
			     (CTint, P.zero, keySpec))
		      | SOME(x,_) => (x, 
				      P.offsetX(P.ptr PTlogicalCT, keySpec), 
				      keySpec)) 
	       end

	   type slice = { keySizeX     : PT.expression, (* slice size*)
			  mungeOffsetX : PT.expression,
			  mungeDescX   : PT.expression, (* mangle expression *)
			  assumedChar  : bool }

	   val defaultSlice = {keySizeX = P.zero, mungeOffsetX = P.zero,
			       mungeDescX = P.zero, assumedChar = false}

	   fun buildKeySlices (kct, kOffsetX, key) : slice list = 
	       let fun errX () = (X.error("Cannot sort by key "^key^
					" because its type: "^
					(CTtoString kct)^ 
					" is not supported for sorting.\n");
				  [defaultSlice])
		   fun slice pt str a = {keySizeX = P.sizeofX pt,
					 mungeOffsetX = kOffsetX,
					 mungeDescX = PT.String str,
					 assumedChar = a} 
		   fun getSlice (sgn,intk,signednessTag) =
		       let val a = (case (intk,signednessTag) of
					(Ast.CHAR,Ast.SIGNASSUMED)=>true
				      | _ =>false)
			   fun slice' pt = 
			   case sgn of 
			       Ast.SIGNED   => slice pt "Lrh" a
			     | Ast.UNSIGNED => slice pt "Lr" a
		       in
		       case intk of
			    Ast.CHAR       => slice' P.char
			  | Ast.SHORT      => slice' P.short
			  | Ast.INT        => slice' P.int
			  | Ast.LONG       => slice' P.long
			  | Ast.LONGLONG   => slice' P.longlong
			  | Ast.FLOAT      => slice  P.float "LrTlh" a
			  | Ast.DOUBLE     => slice  P.double "LrTlh" a
			  | Ast.LONGDOUBLE => slice  P.longdouble "LrTlh" a
		       end
	       in
	       (case CTreduce kct of 
		    Ast.Qual(q,ct') => buildKeySlices(ct', kOffsetX, key)
		  | Ast.Numeric(_,_,sgn,intk,sgnTag) => 
			[getSlice(sgn, intk, sgnTag)]
		  | Ast.StructRef t => 
			let val names = CTgetMemberNames t
			    val PTkct = CTtoPTct kct
			    val ks = map (findKey(PTkct, kct)) names
			    val sliceDescss = map buildKeySlices ks
			    val sliceDescs = List.concat sliceDescss
			    fun incOffset ({keySizeX, mungeOffsetX,
					    mungeDescX,assumedChar}) = 
				{keySizeX = keySizeX, 
				 mungeOffsetX = P.plusX(mungeOffsetX,kOffsetX),
				 mungeDescX = mungeDescX,
				 assumedChar = assumedChar}
			in
			    map incOffset sliceDescs
			end
		  | Ast.EnumRef t => 
			[getSlice(Ast.SIGNED,Ast.INT, Ast.SIGNDECLARED)]
		  | Ast.TypeRef t => X.bug "Impossible!"
		  | Ast.Array(iopt,ct) =>
			let val size = (case iopt of
					    NONE => (errX(); 0)
					  | SOME (i,_) => i)
			    val ks = buildKeySlices(ct, kOffsetX, key);
			    val PTct = CTtoPTct ct
			    fun doOneCell i ([] : slice list) = []
			      | doOneCell i (k::ks) = 
				({keySizeX = #keySizeX k,
				  mungeOffsetX = 
				  P.plusX(#mungeOffsetX k,
					  P.timesX(P.int32X i,P.sizeofX PTct)),
				  mungeDescX = #mungeDescX k,
				  assumedChar = #assumedChar k}
				 :: (doOneCell i ks))
			    fun loop i = if i = size then []
					 else (doOneCell i ks) @ (loop (i+1))
			in
			    loop 0
			end
		  | _ => errX ()
			)
	       end

	   fun processSort( sliceDescs : slice list,
			    PTlogicalCT : PT.ctype) 
	       :  {numSlicesX : PT.expression,  (* number of slices *)
		   slicesX : PT.expression,     (* expression denoting the slice array *)
		   initSs : PT.statement list}  (* statements to declare and initialize array *)
	       =
	       let val wholeRecord = {keySizeX = P.sizeofX(PTlogicalCT),
				      mungeOffsetX = P.zero,
				      mungeDescX = PT.String "",
				      assumedChar = false}
		   val sliceDescs = sliceDescs @ [wholeRecord]
		   val numSlicesX = P.intX (List.length sliceDescs)
		   val (slices, slicesX, slicesDS) = 
		       PTinternalVar("theSlices",
				     P.array(numSlicesX,HRS.sliceCT),NONE)
		   fun initOneSlice (s : slice, (initS,sliceNum)) =
		       let val {keySizeX,mungeOffsetX,mungeDescX,assumedChar}=s
			   val slotX = P.subX(slicesX,P.intX sliceNum)
			   val beginS = 
			       P.assignS(P.dotX(slotX,PT.Id HRS.sliceStart), 
					 mungeOffsetX)
			   val endOffsetX = P.plusX(mungeOffsetX, keySizeX)
			   val endS = 
			       P.assignS(P.dotX(slotX, PT.Id HRS.sliceStop), 
					 endOffsetX)
			   fun genDesAssignS descX = 
			       P.assignS(P.dotX(slotX, PT.Id HRS.slices), descX)
			   val descriptionS = 
			       if assumedChar then
				   PT.IfThenElse
				   (P.charIsUnsignedX,
				    genDesAssignS (PT.String "Lr"),
				    genDesAssignS (PT.String "Lrh")
				    )
			       else 
				   genDesAssignS mungeDescX
		       in
			   ([beginS,endS, descriptionS] @initS, sliceNum + 1)
		       end
		   val (initSs, _) = foldl initOneSlice ([],0) sliceDescs
		   val initSs = slicesDS :: initSs
	       in
		   {numSlicesX = numSlicesX, 
		    slicesX    = slicesX, 
		    initSs     = initSs}
	       end

	   fun processSortList (sortingSpec : (string list) option) 
	       :  {numSlicesX : PT.expression, 
		   slicesX  : PT.expression, 
		   initSs : PT.statement list}
	       =
	       (case sortingSpec of
		    NONE => 
			{numSlicesX = P.zero, slicesX = P.zero, initSs = []}
		  | SOME [] => 
			let val ks = buildKeySlices(logCT, P.zero, "record");
			in
			     processSort(ks,PTlogCT)
			end
		  | SOME sorting => 
			let val ks = map (findKey(PTlogCT,logCT)) sorting
			    val sliceDescss = map buildKeySlices ks
			    val sliceDescs = List.concat sliceDescss
			in 
			    processSort(sliceDescs, PTlogCT)
			end)
	   
	   val sortSpecs = processSortList sort
		   
	   fun cnvEvtInfo (n,ct,HI.Base) = [([n],ct)]
	     | cnvEvtInfo (n,ct,HI.Hier h) = 
	       let val subPaths = cnvEvtInfoList (#elts h)
	       in
		   List.map (fn (path,ct)=> (n::path,ct)) subPaths
	       end
	   and cnvEvtInfoList es = List.concat(List.map cnvEvtInfo es)
	       
	   (* Process the withevents clause *)
    	   (* Extract the type of events, 
	    the elements of the union,
	    the window type, the window size, the window Offset,
            and the expression to detect the next event given the window. *)

	   val (PTeventCT,eventElts,PTwindowCT,
                windowSize,windowOffset, detectFn, implicit) =
                  case detector 
                  of SOME (H.DetectFun s) => let
	             val detector = PT.Id s
                     val (detectCT,_) = cnvExp(detector)
 		     val (retCT,argCT) = 
		       case getFunction detectCT of
			   SOME (r,[arg]) => (r,arg)
			 | SOME _ => X.fail ("withevents function must "^
					   "take exactly one argument.")
			 | _ => X.fail ("unexpected non-function type "^
				      (CTtoString detectCT)^".")
		     val strCT = AST.Pointer logCT
		     fun err s = 
		         X.error ("Event detection argument type "^ s^
			          " must be compatible "^
			          "with stream element type "^
			          (CTtoString strCT)^".\n")
		       
  		     val (winSize,winOff) =
		       case CThinfo argCT of
			   SOME (HI.Window(ct,size,offset)) =>
			       if(isAssignable(ct,strCT,NONE))
				   then (size,offset)
			       else (err (CTtoString ct); (size,offset))
			   | _ => (X.error ("withevents function must take "^
				   	    "a windowed array type. "^
					    (CTtoString detectCT));
				   (0,0))
				   
		     val evtElts = cnvEvtInfoList (#elts (CTeventInfo retCT))
  	             in
		       (CTtoPTct(retCT),evtElts,
		        CTtoPTct(argCT),winSize,winOff,
                        fn winId => PT.Call(detector, [winId]), false)
	             end (* end H.DetectFun *)
                  | SOME (H.DetectExpr(s,winSize,winOff,body)) => let
                    val anEvtPth = (case events of [] => X.fail ("Must have event "^
	        						 "clause when using "^
								 "event detection "^
								 "expressions.\n")
                                                   | x::xs => (#name x))
                    val anEvtName = P.pathToString anEvtPth
                    val {name = munionName, 
                         elts = elts} = case (CThinfo (CTfromTypeName'(anEvtName,
	                                               "is not a member of a declared "^
                                                       "multi-union.\n ")))
                                             of SOME(HI.EventTy e') => e'
                                             | _ => X.fail ("Unrecognized multi-union "^
          					           "element "^(anEvtName))
                    val retType = P.makeCT [PT.TypedefName munionName]
                    val evtElts = cnvEvtInfoList(elts)
                    val PTwindowCT = P.array(winSize,P.ptr PTlogCT)
                    val accessX = fn winId => 
                           HPTS.substExp (s,winId,body)
                    val winSizeInt = cnvStaticInt(winSize)
                    val winOffInt = cnvStaticInt(winOff)
                    in
                        (retType, evtElts, PTwindowCT,
                         winSizeInt, winOffInt, accessX, false)
                    end
		| NONE => (
		    let val evtName = defEvent
			val {name=munionName, elts = elts} =
			    (case CThinfo(CTfromTypeName' (evtName, ""))
			    of SOME(HI.EventTy e') => e'
                            | _ => X.bug "missing munion decl.\n"
				  (* end case *))
		       val retTyp = P.makeCT [PT.TypedefName munionName]
		       val evtElts = cnvEvtInfoList(elts)
		       val PTwindowCT = P.array(P.intX 1, P.ptr PTlogCT)
		       val accessX = fn winId => 
			   PT.ExprExt(H.HEventC [(evtName, 
				       SOME (P.subX( winId, P.zero)))])
		    in 
			(retTyp, evtElts, PTwindowCT,
			 1, 0, accessX, true)
		    end
			   (* end NONE case *))

	   val PTrecordPtrCT = P.makeCT [PT.Pointer PTlogCT]
	   val PTrecordCT    = PTlogCT

	   (* A ton of local variable declarations and initializations *)
	   val (eosVar,eosId,eosDecl) = 
	       PTinternalVar("eos",P.int,SOME (P.intX 1))
	   val (offsetVar,offsetId,offsetDecl) =
	       PTinternalVar("offset",P.int,SOME (P.intX 0))
	   val (windowVar,windowId,windowDecl) =
	       PTinternalVar("window",PTwindowCT,SOME (PT.InitList [P.intX 0]))
	   val (winValueVar,winValueId,winValueDecl) =
	       PTinternalVar("winV",P.array(P.intX windowSize,PTrecordCT),NONE)
	   val (currVar,currId,currDecl) =
	       PTinternalVar("nextV",P.int,SOME (P.intX windowOffset))
	   val (evtVar,evtId,evtDecl) =
	       PTinternalVar("evt",PTeventCT,NONE)
	   val (streamVar,streamId,streamDecl) =
	       PTinternalVar("stream",HRS.streamCT,NONE)

	   val explainCurr = (currVar ^ 
			      " contains the offset in " ^
			      winValueVar ^
			      " where the next stream element will be stored.")
	   val explainOffset = (offsetVar ^
				" stores the number of elements to be read" ^
				" from the window.")


	   (* Separate declarations from bodies for each event. 
	    * Also converts event arguments into variable declarations. 
	    *)
	   fun processEvtBlock validEvts ({arg,body,name} : HEvent) =
	       let fun eq (e: string list * ctype) = (#1 e = name)
		   val nameString = P.pathToString name
		   val (expectedCT,remainingEvts, name) = 
		       case HL.extract eq validEvts of
			   SOME ((_,ct),rest) => (ct,rest,name)
			 | NONE => if implicit 
			       (*  andalso  (nameString = "") *)
			           then (Ast.Pointer logCT, [], [defEvent])
				   else
					X.fail ("Duplicate or unrecognized event "^
					 nameString)
		   val body = 
		       case arg of
			   NONE => body
			 | SOME (argCT,argDecr) =>
			       let val (ct,nameOpt) = CTcnvDecr(argCT,argDecr)
				   fun err ()= X.error "Unnamed event parameter."
				   val argName = 
				       case nameOpt of 
					   NONE => (err (); "BOGUS")
					 | SOME n => n
				   val _ = 
				       if isAssignable(ct,expectedCT,NONE) 
					   then ()
				       else X.error("Event "^nameString^
						  " argument type ("^
						  (CTtoString ct)^ 
						  ") is incompatible "^
						  "with declared type ("^
						  (CTtoString expectedCT)^
						  ").")
				   val argDecl = 
				       P.ctDeclS(argCT,argDecr,
						 P.evtValueHX evtId name)
			       in
				   P.compoundS(argDecl::body::[])
			       end	

		   val bodyS = PT.IfThen(P.evtFlagHX evtId name, body)
	       in
		   (bodyS,remainingEvts)
	       end

	   val blocks =
	       let fun loop (rest,[],bs) = List.rev bs
		     | loop (rest,hd::tl,bs) = 
		   let val (bs',rest') = processEvtBlock rest hd
		   in
		       loop (rest', tl, bs'::bs)
		   end
	       in
		   loop(eventElts,events,[])
	       end

	   val decls = [eosDecl, offsetDecl, windowDecl, winValueDecl,
			evtDecl, streamDecl, currDecl]

	   (* Below here we begin actually constructing pieces of the output.*)

	   val isStreamEmptyX = P.eqX(eosId, P.zero)
	   val isStreamNotEmptyX = P.notX isStreamEmptyX

	   val toStreamS = 
	       case streamInfo of
		   Stream=> P.assignS(streamId, streamExp)
		 | Map (mi,low,high) =>
		       let val ct = #PTkeyCT mi 
			   val (_,lowId,lowDecl) = 
			       PTinternalVar("low_tmp",ct,NONE)
			   val (_,hiId,hiDecl) =
			       PTinternalVar("high_tmp",ct,NONE)
			   val mapId = streamExp
			   val toStreamX = HRS.mapToStream(mapId,lowId,hiId, 
							  P.sizeofX(PTlogCT))
			   val initRs = case (low,high) 
			       of (SOME l, SOME h) => [
					P.assignS(lowId,l),
					P.assignS(hiId,h)]
			       |  (NONE,NONE) => 
				   [PT.Expr(HRS.mapRange(mapId,
							 P.addrX lowId,
							 P.addrX hiId))
				    ]
			       | _ => X.bug "Both map ranges or neither must be given."
in
			   P.compoundS ([lowDecl,
					 hiDecl] @
			                initRs @
					[P.assignS(streamId,toStreamX)])
		       end

	   val streamStartS =
	       let val {initSs,numSlicesX,slicesX} = sortSpecs in
		   P.compoundS (initSs @
				[PT.Expr (HRS.streamStart(streamId,
							 wrapperX,
							 numSlicesX,
							 slicesX))])
	       end

	   fun windowX i = P.subX(windowId,i)
	   fun winValueX i = P.subX(winValueId,i)

	   val shiftWindowS =
	       let fun loop i =
		   if(i<(windowSize-1)) then
		       (P.assignS(windowX (P.intX i),
				  windowX (P.intX (i+1)))) :: 
		       (loop (i+1))
		   else []
	       in
		   P.compoundS (loop 0)
	       end
	
	   val incCurrS = 
	       P.assignS(currId,
			 P.modX(P.plusX(currId,P.intX 1),P.intX windowSize))

	   val nextS =
	       let val activeRecord = P.addrX(winValueX currId) 
	       in		 
		   P.assignS(eosId,HRS.streamNext(streamId,activeRecord)) 
	       end

	   (* Fill the window enter the loop *)
	   val initiallyS = 
	       let val winOffsetX = P.plusX(offsetId,P.intX windowOffset)
		   val bodyS = 
		       P.compoundS [nextS,
				    PT.IfThen(isStreamEmptyX,
					      PT.Break),
				    P.assignS(windowX winOffsetX, 
					      P.addrX (winValueX currId)),
				    PT.Expr (P.postIncX offsetId),
				    incCurrS
				    ]
	       in
		   PT.While(P.ltX(offsetId,P.intX (windowSize-windowOffset)),
			    bodyS)
	       end

	   val decrOffsetS = 
	       let val emptyS =
		   P.compoundS [PT.Expr (P.postDecX offsetId),
				P.assignS(windowX (P.intX (windowSize-1)), 
					  P.zero)] 
		   val nonEmptyS = 			     
		       P.compoundS [P.assignS(windowX (P.intX (windowSize-1)),
					      P.addrX (winValueX currId)),
				    incCurrS
					  ]
	       in
	       PT.IfThenElse(isStreamEmptyX,emptyS,nonEmptyS)
	       end  

	   val detectS = P.assignS(evtId, detectFn(windowId))

	   val iterateS =
	       let val loopBody =
		       P.compoundS (
				    (detectS ::
				     (P.commentS "Event blocks") ::
				     blocks @
				     (P.commentS "Finished event blocks") ::
				     shiftWindowS           ::
				     nextS                  ::
				     decrOffsetS            ::
				     []))
		   val mainLoop = PT.While(P.neqX(offsetId,P.zero),
					   loopBody)
	       in
		   P.compoundS (decls @ 
				[toStreamS,
				 streamStartS,
				 P.commentS explainCurr,
				 P.commentS explainOffset,
				 P.commentS "Pre-load the window",
				 initiallyS,
				 P.commentS "Main loop",
				 mainLoop,
				 P.commentS "End of main loop"])
	       end
       in
	   cnvStmt iterateS
       end 

(* Conversions : Expressions *************************************************)
    fun CNVExp exp =
	let 
	    (* get the hancock operator associated with the lhs *)
	    fun getHop e =
		(case e of
		     PT.Binop(PT.OperatorExt(oper),e1,e2) => SOME(oper,e1,e2)
		   | PT.MARKexpression (el,e) => getHop e
		   | _ => NONE)

	    fun LHSfail opString = X.fail (opString ^ "not supported on lhs.")

	    fun cnvIndexLHS ((lhs1,lhs2,rhs),modOp) =
		let val _ = (case modOp of
				 NONE => () 
			       | SOME _ => X.fail "<: :> operation does not yet support compound assignment operators.")
		    val {rangeCT,keyCT,keyX,keyDecls} = chkIndex(lhs1,lhs2, true)
		    val (rhsCT,rhsAst) = cnvExp rhs
		    val _ = 
			(* XXX - I am not sure what the right test
			 is here.  Not assignable because of arrays
			 Arrays are never assignable but arrays
			 could be legal here!?
			 *)
			if ( equalType(rangeCT,rhsCT)  orelse
			     isAssignable(rangeCT,rhsCT,NONE))
			    then if sizeOf(rhsCT) > sizeOf(rangeCT)
				then X.warn ("Type of right hand side "^
					     "("^ (CTtoString rhsCT) ^")"^
					     " is larger than type of left "^
					     "hand side ("^ 
					     (CTtoString rangeCT)^").")
				 else ()
			else X.fail ("Type "^
				   (CTtoString rhsCT)^
				   " is not equal to type "^
				   (CTtoString rangeCT))
		    val (tk,tkSym,tkId,tkDecl) = declTemp("key",keyCT)
		    val initKeyX = P.assignX(PT.Id tk, keyX)
		    val indexX = P.param(HRS.keyCT,  PT.Id tk)
		    fun putX e = P.commaX(initKeyX,
					  P.commaX(HRS.mapPut(lhs1,indexX,P.addrX e),e))
		    val (t,tSym,tId,tDecl) = declTemp("index",rangeCT)
		    val _ = bindDecls (keyDecls @ [tkDecl])
		    val _ = HS.addDecl tkDecl
		in
		    if P.isExpAddressable rhs then
			cnvExp (putX rhs)
		    else 
			(bindDecls [tDecl];
			 HS.addDecl tDecl;
			 cnvExp(P.commaX(P.assignX(PT.Id t,rhs),
					 putX (PT.Id t))))
		end

	    fun cnvEventValueLHS (evtInfo : HI.EventInfo, 
				  lhs1ct, lhs1, lhs2, rhs) =
		let val elt = (case lhs2 of
				   PT.Id i => i
				 | _ => X.bug "Expect id right of . ")
		    fun f (x,_,_) = (elt = x)
		    val eltInfo = List.find f (#elts evtInfo)
		in
		    case eltInfo of
			NONE => X.fail ("Munion contains no element named "^
				      elt)
		      | SOME _ => 
			    let val tmp = declBindTemp ("evt",
							Ast.Pointer lhs1ct,
							false)
				val tmpId = PT.Id tmp
				val e1 = P.assignX(tmpId, P.addrX lhs1)
				fun dot id = P.dotX(P.starX tmpId,id)
				val e2 = 
				    P.assignX(dot (P.mkEvtFlagId elt),
						   P.intX 1)
				val e3 = P.assignX(dot (P.mkEvtValueId elt),
						   rhs)
				val final = (P.commaX(e1,
						      P.commaX(e2,e3)))
				    
			    in
				cnvExp final
			    end
		end

	    fun cnvDirDotLHS (di,ct1,lhs1,lhs2,rhs) =
		let val _ =  if isConst ct1
			     then (X.error ("Cannot assign to a field in "^
				          "a const directory.\n"))
			     else ()
		in
		    cnvExp
		    (PT.ExprExt(H.HAssign 
				(P.assignX(P.dotX(P.starX lhs1,lhs2),
					   rhs))))
		end

	    fun cnvDotLHS ((lhs1,lhs2,rhs),modOp, originalExp) =
		let val (ct1,ast1) = cnvExp lhs1
		in 
		    case (modOp,CThinfo ct1) of
			(NONE,SOME (HI.EventTy ei)) => 
			    cnvEventValueLHS(ei,ct1,lhs1,lhs2,rhs) 
		      | (NONE,SOME (HI.Dir di)) =>
			    cnvDirDotLHS(di,ct1,lhs1,lhs2,rhs)
		      | _ => cnvExp originalExp
		end

	    fun cnvArrowLHS ((lhs1,lhs2,rhs),modOp) =
		let val oper = P.assignModifierOp modOp 
		in
		    cnvExp 
		    (PT.ExprExt(H.HAssign 
				(PT.Binop(oper,
					 PT.Binop(PT.OperatorExt H.HDot, 
						  P.starX lhs1,lhs2),
					 rhs))))
		end

	    (* modOp is the operator modifying assignment. 
	     i.e. SOME + for += *)
	    fun cnvAssign (modOp,hop,x,originalExp) =
		case hop of
		    H.HView      => LHSfail "$"
		  | H.HUnion     => LHSfail ":+:"
		  | H.HDiff      => LHSfail ":=:"
		  | H.HRemove    => LHSfail ":\\:"
		  | H.HMember    => LHSfail "@"
		  | H.HMapRemove => LHSfail "\\<: :>"
		  | H.HMapTestKey  => LHSfail "?<: :>"
		  | H.HMapMember => LHSfail "@<: :>"
		  | H.HIndex     => cnvIndexLHS (x,modOp)
		  | H.HDot   => cnvDotLHS(x,modOp,originalExp)
		  | H.HArrow => cnvArrowLHS (x,modOp)

	    fun cnvEventC [] = X.bug "Impossible empty multi-union constructor."
	      | cnvEventC (elts as (firstElt :: _)) = 
		(* 
		 Get the eventinfo from the first element.  Type-check w.r.t.
		 that.

		 For each element check that 
		     it is bound to some eventinfo
		     the type of the expression if any matches.
		     Generate the assignment to a floated temporary.
		     
		 
		 *)
		let val {name=eventName,elts=eventElts} = 
		    case CThinfo (CTfromTypeName' (#1 firstElt,
                                  "is not a member of a declared multi-union.\n")) 
		      of SOME (HI.EventTy e') => e'
		      | _ => X.fail ("Unrecognized multi-union element "^(#1 firstElt))

		    val (t,tSym,tId,tDecl) = 
			declZeroedTemp("evt", CTfromTypeName eventName)
		    val tX = PT.Id t
		    fun genElt (field,ct,evtElt) NONE =
			((if not (equalType(ct,AST.Void)) then
			      X.error ("Element " ^ field ^
				     " must be assigned a value.")
			  else ());
			      P.initEventFlagX tX field true)
		      | genElt (field,ct,evtElt) (SOME x) = 
			let val (ct',x') = cnvExp x
			    val _ = if isAssignable(ct,ct',SOME x') then () 
				    else X.error ("Initializer for element " ^ 
						field ^
						" has improper type")
			in
			    case evtElt of HI.Base => 
				P.commaX(P.initEventFlagX tX field true,
					 P.initEventValueX tX field x)
				| _ => P.initEventValueX tX field x
			end
		    fun loop evtElts [] = X.bug "Empty munion constructor."
		      | loop evtElts ((name,expOpt)::tl) =
			let fun f (n,_,_) = (n=name)
			    val (pertinent,rest) = List.partition f evtElts
			    val exp =
				case pertinent of
				    [hd] => genElt hd expOpt
				  | [] => X.fail ("Duplicate or non-existent " ^
						"munion element " ^ name)
				  | _ => X.bug ("Impossible munion type with " ^
					      "duplicate fields.")
			in
			    case tl of
				[] => exp
			      | _ => P.commaX(exp,loop rest tl)
			end
		    val finalExp = P.commaX(loop eventElts elts, tX)
		in
		    bindDecls [tDecl];
		    HS.addDecl tDecl;		    
		    cnvExp finalExp
		end

	    fun cnvEventEmpty PTct =
		let val (ct,_) = CTcnvType PTct 
		    val _ = CTeventInfo ct
		    val (t,tSym,tId,tDecl) = 
			declZeroedTemp("evt", ct)
		in
		    bindDecls [tDecl];
		    HS.addDecl tDecl;		
		    cnvExp(PT.Id t)		    
		end

	in
	    case exp of 
		H.HAssign (e as PT.Binop(oper,lhs,rhs)) =>
		    (if P.isAssignOp oper then
			 (case (oper,getHop lhs) of
			      (_,NONE) => cnvExp e
			    | (oper,SOME (hop,e1,e2)) => 
				  cnvAssign(P.assignOpModifier oper,
					    hop,(e1,e2,rhs),e))
		     else (X.bug "Hassign not wrapped around assignment operator"))
	      | H.HAssign _ => (X.bug "HAssign not wrapped around binop.")
	      | H.HEventEmpty ct => cnvEventEmpty ct
	      | H.HEventC c => cnvEventC c
	end



        fun cnvInitDecl (PT.MARKdeclaration (e,d)) : PT.declaration = 
	    let val d' = cnvInitDecl d
	    in
		PT.MARKdeclaration(e,d')
	    end
	  | cnvInitDecl (d as PT.DeclarationExt _) = X.fail "unexpected decl ext.\n"
	  | cnvInitDecl (PT.Declaration(dt,des)) =
	    let 
	    val (mode,specs') = P.extractMode (#specifiers dt)
	    val readonly = List.exists P.isConst (#qualifiers dt)
	    val dt = {specifiers = specs',
		      qualifiers = #qualifiers dt, 
		      storage = #storage dt}

	    (* convert string to initialization expression for Hancock types. *)
	    fun PTcnvHInit(ct, initexp, mode, nameOpt) : PT.expression =
		let val name = case nameOpt of NONE => "Anonymous"
		               | SOME n => n
		    val (hinfo, params) = getHTyInfo ct 
		in 
		    case hinfo 
		      of NONE => initexp
		    | SOME hi => 
			let val (initCT,_) = cnvExp initexp
			    val isString = CTisString initCT
			    val isZero = PTisZero initexp
			in
			    if isString  then 
				let fun cnv (HI.Map mi) = cnvInitH(#formalParams mi, #initFun mi, 
								   #initFunNoParams mi,
								   initexp, readonly,mode, params, name)
				      | cnv (HI.Pickle pi) = cnvInitH(#formalParams pi, #initFun pi, 
								      #initFunNoParams pi,
								      initexp, readonly,mode, params, name)
   		                      | cnv (HI.Dir di) = cnvInitH(#formalParams di,
								   #initFun di, #initFunNoParams di,
								   initexp, readonly,mode, params, name)
				      | cnv (HI.Stream si) = cnvInitH(#formalParams si,
								      #initFun si, #initFunNoParams si,
								      initexp, readonly, mode, params, name)
				      | cnv _ = initexp
				in 
				    cnv hi 
				end
			    else if isZero andalso isGenerativeStream hi then
				let val si = case hi of HI.Stream si => si 
			                             | _ => raise X.bug "Expected stream.\n"
				in
				    cnvInitH(#formalParams si,
					     #initFun si, #initFunNoParams si,
					     initexp, readonly, mode, params, name)
				end
                            else
				let val _ = if not(mode = H.Either) 
						then X.error ("Hancock types with new or exist "^
							    "modifiers must be initialized " ^
							    "from a string.\n")
					    else ()
				in
					initexp
				end
			end
		end

	    fun doOne (d as (decr,PT.EmptyExpr)) = d
	      | doOne (decr, PT.MARKexpression (loc,e)) = 
		let val (decr', e') = doOne (decr,e) 
		in 
		    (decr', PT.MARKexpression(loc, e'))
		end
	      | doOne (d as (decr,PT.InitList es)) = d
	      | doOne (decr,e) =
		let 
                    val (ct,so) = CTcnvDecr(P.dtToCT dt ,decr)
		    val initExp = PTcnvHInit(ct,e,mode, so)
		in
		    (decr, initExp)
		end
	    val des' = List.map doOne des
	in
	    PT.Declaration(dt, des')
	end

	
    fun CNVStat stmt =
	let fun cnvCompound (PT.Compound ss) =
		let val (ds,ss) = List.partition P.isDecl ss
		    (* Converts string initializers into appropriate function
		     calls to HRS runtime.
		     Because it may convert the initializing expression, we 
		     must wrap these in a dummy pushDecls/popDecls. *)
		    fun splitDecl (PT.Decl d) =
			let val d' = cnvInitDecl d
			in
			    PT.Decl d'
			end
		      | splitDecl _ = X.bug "Expected declaration"
		    val _ = HS.pushDecls ()
		    val ds'  = List.map splitDecl ds
		    val _ = ignore(HS.popDecls ())
		    val s = PT.Compound(ds' @  ss)		
		    val _ = HS.pushDecls ()
		    val astStmt = cnvStmt s 
		    val decls = HS.popDecls ()
		in
		    case astStmt of
			Ast.STMT(Ast.Compound(d,s),aid,loc) => 
			    Ast.STMT(Ast.Compound(decls@d,s),aid,loc)
		      | Ast.STMT(s,aid,loc) => 
			    (X.bug "Compound statement did not compile to AST.Compound")
		end
	      | cnvCompound _ = X.bug "Expected compound statement"

	    fun cnvCopy(to,from) =
		let val (toCT,_) = cnvExp to
		    val (fromCT,_) = cnvExp from			
		    val _ = if isConst toCT then X.fail "LHS of :=: is const.\n" else ()
                    val errorMsg = ("Left-hand side of :=: has type "^
			            (CTtoString toCT)^
			             "; the right-hand side has type "^
				     (CTtoString fromCT)^
			            ". Expected the same type.\n")
                    fun getInfo (toCT, fromCT) = let
                        val (hinfo, tyParams) = getHTyInfo toCT
                        in
                        case hinfo 
			    of SOME (HI.Map mTo) => 
				(case getHTyInfo fromCT
				     of (SOME (HI.Map mFrom),tyParams) => 
					 (#tid mTo, #tid mFrom, 
					  HRS.mapCopy(to, from))
				   | _ => X.fail errorMsg)
			  | SOME (HI.Pickle pTo) => 
				(case getHTyInfo fromCT
				     of (SOME (HI.Pickle pFrom),tyParams) => 
					 (#tid pTo, #tid pFrom, 
					  HRS.pickleCopy(to, from))
				   | _ => X.fail errorMsg)
			  | SOME (HI.Dir dTo) => 
				     (case getHTyInfo fromCT
					  of (SOME (HI.Dir dFrom),tyParams) => 
					      (#tid dTo, #tid dFrom, 
					       PT.Call(PT.Id (#copyFun dTo),
						       [to, from]))
					| _ => X.fail errorMsg)
			  | _ => X.fail "Operator :=: implemented only for directories, pickles, and maps.\n"
			 end
		    val (ToTid, FromTid, copyX) = getInfo(toCT, fromCT)
		in
		    if ToTid = FromTid then
			let val (_,ast) = cnvExp (copyX)
			in
			    wrapStmt (AST.Expr (SOME ast))
			end
		    else X.fail errorMsg
		end
	in
	  case stmt of
	      H.HCompound s => cnvCompound s
	    | H.HMapCopy(e1,e2) => cnvCopy(e1,e2)
	    | H.HIterate i => cnvIterate i
	end

(* Conversions : binary operators ********************************************)
    fun CNVBinop {binop=binop, arg1Expr=e1, arg2Expr=e2} =
	let fun wrapId (i as {ctype=ct, ...}:Ast.id) = wrapExp(ct,Ast.Id i)

	    fun getId (PT.Id str) = str
	      | getId _ = X.bug "Expected id."


	    fun cnvView (e1,e2) =
		let val view = getId e2
		    val (ctE1,astE1) = cnvExp e1
		    val {recName = recName, v1Name = v1Name, v1CT = v1CT, 
			 v2Name = v2Name, v2CT = v2CT, 
			 toView1 = toView1, toView2 = toView2 } = 
			(case CThinfo ctE1 of
			    SOME(HI.Record r) => r
			  | _ => X.fail ((CTtoString ctE1) ^
				       " is not a view."))		       
		    fun genView view exp =
			let val (pt,ds) = view exp
			in
			    HS.addDecls ds;
			    bindDecls ds;
			    cnvExp pt
			end
		    val _ = 
			if topLevel () then
			    X.fail "$ cannot be used outside a function body"
			else ()
		in
		    if view = v1Name then 
			genView toView1 e1
		    else if view = v2Name then genView toView2 e1
			else if view = recName then
			    X.fail ("Conversion to views unimplemented.")
			     else X.fail ("Unrecognized view " ^ view)
		end

	    fun cnvIndex (e1,e2) =
		let
		    (* e1 must be a map, e2 of key type. *)
                    val {rangeCT,keyCT,keyX,keyDecls} = chkIndex(e1,e2, false)
		    val (t,tSym,tId,tDecl) = declTemp ("index",rangeCT)
		    val (tk,tkSym,tkId,tkDecl) = declTemp ("key",keyCT) 
		    val addrT = PT.Unop(PT.AddrOf,PT.Id t)
                    val tkInitX = P.assignX(PT.Id tk, keyX)
                    val indexX = P.param(HRS.keyCT, PT.Id tk)
		    val getC = HRS.mapGet(e1,indexX,addrT)
                    val getX = P.commaX(tkInitX,getC)
		    val _ = bindDecls (keyDecls @ [tDecl,tkDecl])
		    val _ = HS.addDecl tDecl
		    val _ = HS.addDecl tkDecl
		    val (ct,ast) = cnvExp getX
		    val (_,tExp) = wrapId tId
		in
		    wrapExp(rangeCT,Ast.Comma(ast,tExp))
		end

	    (* Check that e1 and e2 have the same event type, and return the
	     eventInfo and the type. *)
	    fun verifySameEvent (e1,e2) =
		let val (lCT,_) = cnvExp e1
		    val (rCT,_) = cnvExp e2
		    val _ = if isAssignable(lCT,rCT,NONE) then ()
			    else X.error ("The type of the left-hand operand to :+: ("^
					CTtoString(lCT) ^ ") does not match the type " ^
					"of the right-hand operand ("^
					CTtoString(rCT)^").\n")
		    val eventInfo = CTeventInfo rCT
		in
		    (rCT,CTeventInfo rCT)
		end


	    fun cnvUnion (e1, e2) = 
		let val (rCT,eventInfo) = verifySameEvent(e1,e2)

		    val trslt = declBindTemp("result",rCT,false)
                    val trsltInitX = P.assignX(PT.Id trslt, e1)

		    fun isExplicit(e) = ( case e 
		      of PT.MARKexpression(l,e') => isExplicit(e')
                      |  PT.ExprExt(e') => (case e'
                                            of H.HEventC(l) => SOME l
                                            |  _ => NONE
			  	            (* end case *))
                      | _ => NONE
                        (* end case *))
                    
                    val rhsX = (
                        case isExplicit(e2) 
                        of NONE => (let 
		           val trhs = declBindTemp("rhs",rCT,false)
                           val trhsInitX = P.assignX(PT.Id trhs, e2)

  		           fun doOne rhsX resultX (name,_,HI.Base) = 
			    P.condX(P.evtFlagX rhsX name,
				P.commaX(P.initEventFlagX resultX name true,
					 P.commaX(P.initEventValueX resultX name 
						  (P.evtValueX rhsX name),
						  resultX)),
				resultX)
		            | doOne rhsX resultX (name,_,HI.Hier {elts,...}) = 
 		                 doList (P.evtValueX rhsX name) 
					(P.evtValueX resultX name) elts

                           and doList rhsX resultX [] = resultX
                             | doList rhsX resultX (e::es) = 
		        	let val rest = doList rhsX resultX es
			        in
			          P.commaX(doOne rhsX resultX e, rest)
			        end
                           val initX = doList (PT.Id trhs) (PT.Id trslt) (#elts eventInfo)
			   in
			     P.commaX(trhsInitX, initX)
                           end (* end NONE *))
                        | SOME l => (let
                           fun doOne resultX accX (s,eOpt) = 
                                 P.commaX(P.initEventFlagX resultX s true,
				   P.commaX(P.initEventValueX resultX s
                                     (valOf eOpt), accX))
                           fun doList resultX [] = resultX
                             | doList resultX (e::es) = let
				 val rest = doList resultX es
                                 in 
				    doOne resultX rest e
                                 end
(*

                           fun doList resultX [] = resultX
                             | doList resultX [e] = doOne resultX resultX e
                             | doList resultX (e::es) =
                                let val rest = doList resultX es
                                in
				  P.commaX(doOne resultX e, rest)
				end
*)
                           in
			       doList (PT.Id trslt) l
                           end (* end SOME *))
                           
                          (* end case *))

		    val unionX = P.commaX(trsltInitX,rhsX) 

		in
		    cnvExp unionX
		end

	    fun cnvDiff(e1,e2) =
		let val(ct,eventInfo) = verifySameEvent(e1,e2) 
		    val t1 = declBindTemp("t1",ct,false)
		    val t2 = declBindTemp("t2",ct,false)
		    fun t1Dot f = P.evtFlagX (PT.Id t1) f
		    fun t2Dot f = P.evtFlagX (PT.Id t2) f
		    fun present f = P.andX(t1Dot f, t2Dot f)
		    fun diffOne (f,_,_) = P.condX(present f,
						  P.assignX(t1Dot f,P.intX 0),
						  P.intX 0)
		    val finalExp = 
			P.commaXs ([P.assignX(PT.Id t1,e1),
				    P.assignX(PT.Id t2,e2)] @
				   (List.map diffOne (#elts eventInfo)) @
				   [PT.Id t1])
					      
		in
		    cnvExp finalExp
		end

	    fun cnvEventMember (source,elt) =		
		let val (ct,ast) = cnvExp source
		    val evtInfo = CTeventInfo ct
			     
		    fun toPath (PT.Id x) = [x] 
		      | toPath (PT.Binop(PT.Dot,e1,PT.Id s)) = (toPath(e1)@[s])
		      | toPath (PT.MARKexpression(_,e)) = toPath(e)
		      | toPath _ = X.fail ("Illegal expression form.")
					
		    val path = toPath elt

		    fun gen x ({elts=evtElts,name=n}:HI.EventInfo) (hd::tl) =
		    let fun f (n,_,_) = (n=hd)			
		    in
			case List.find f evtElts of
			    NONE => X.fail ("Multi-union "^n^
					  " has no element named "^
					  hd)
			  | SOME (st,_,HI.Hier evtInfo) =>
				gen  (P.dotX (x, P.mkEvtValueId hd)) evtInfo tl
			  | SOME (st,_,HI.Base) =>
				(case tl of
				     [] =>  (P.dotX (x, P.mkEvtFlagId hd))
				   | _ => X.fail("Munion element "^hd^
					       " is bottom-most element."))
		    end
		      | gen x _ [] = (X.error ("Support for checking emptiness "^
					     "of an munion unimplemented");
				      x)
		in
		    cnvExp(gen source evtInfo path)
		end

	    fun cnvMapMember(mapX,atX) = 
		let val (ct,ast) = cnvExp mapX
		    val mi = CTmapInfo ct

		    val {rangeCT,keyCT,keyX,keyDecls} = chkIndex(mapX,atX, false)
                    val (tk,tkSym,tkId,tkDecl) = declTemp("key",keyCT)
                    val tkInitX = P.assignX(PT.Id tk, keyX)
                    val kIndexX = P.param(HRS.keyCT, PT.Id tk)
                    val testX = HRS.mapQuery(mapX,kIndexX)
                    val compX = P.commaX(tkInitX,testX)
                    val _ = bindDecls(keyDecls @[tkDecl])
                    val _ = HS.addDecl tkDecl
		in
		    cnvExp compX
		end

	    fun cnvEventValue(evtInfo:HI.EventInfo,e1,e2) =
		let val elt = (case e2 of
				   PT.Id i => i
				 | _ => X.bug "Expect id right of . ")
		    fun f (x,_,_) = (elt = x)
		    val eltInfo = List.find f (#elts evtInfo)
		in
		    case eltInfo of
			NONE => X.fail ("Munion contains no element named "^
				      elt)
		      | SOME _ => cnvExp (P.evtValueX e1 elt)
		end

	    fun cnvDirMember (di,e1,e2) = cnvExp (P.dotX(P.starX e1,e2))


	    fun cnvDot(e1,e2) =  
		let val (ct1,ast1) = cnvExp e1
		in 
		    case CThinfo ct1 of
			SOME (HI.EventTy ei) => cnvEventValue(ei,e1,e2) 
		      | _ => cnvExp (P.dotX(e1,e2))
		end

	    fun cnvArrow(e1,e2) = cnvExp (PT.Binop (PT.OperatorExt H.HDot,
						    P.starX e1,
						    e2))

	    fun cnvRemove(e1,e2) = 
		let val (ct1,ast1) = cnvExp e1
		    val evtInfo = CTeventInfo ct1
		    val elt = (case e2 of
				   PT.Id i => i
				 | _ => X.bug ("Expected munion element " ^
					     "right of :\\:"))
		    val tmp = declBindTemp ("evtrm",ct1,false)
		    val tmpId = PT.Id tmp
		    val e1' = P.assignX(tmpId,e1)
		    val e2' = P.initEventFlagX tmpId elt false 
		    val final = P.commaX(e1',P.commaX(e2',tmpId))
		in
		    cnvExp final
		end

	    fun cnvMapOp f (mapX, keyX) =
		let val (ct,ast) = cnvExp mapX
		    val mi = CTmapInfo ct

		    val {rangeCT,keyCT,keyX,keyDecls} = chkIndex(mapX,keyX,false)
                    val tk = declBindTemp("key",keyCT,false)
		    val tkId = PT.Id tk
                    val tkInitX = P.assignX(tkId, keyX)
                    val kIndexX = P.param(HRS.keyCT, tkId)
                    val testX = f(mapX,kIndexX)
                    val compX = P.commaX(tkInitX,testX)
                    val _ = bindDecls(keyDecls)
		in
		    cnvExp compX
		end


	    fun cnvMapRemove(mapX,keyX) =
		  cnvMapOp HRS.mapRemove (mapX, keyX)

	    fun cnvMapTestKey(mapX, keyX) =
		cnvMapOp HRS.mapTestKey(mapX, keyX)

	in
	    case binop of
		H.HView   => cnvView (e1,e2)
	      | H.HIndex  => cnvIndex (e1,e2)
	      | H.HUnion  => cnvUnion(e1,e2)
	      | H.HDot    => cnvDot(e1,e2)
	      | H.HArrow  => cnvArrow(e1,e2)
	      | H.HDiff   => cnvDiff(e1,e2)
	      | H.HRemove => cnvRemove(e1,e2)
	      | H.HMember => cnvEventMember(e1,e2)
	      | H.HMapRemove => cnvMapRemove(e1,e2)
	      | H.HMapMember => cnvMapMember(e1,e2)
	      | H.HMapTestKey => cnvMapTestKey(e1,e2)
	end


(* Conversions : external declarations  **************************************)
    fun CNVExternalDecl decl =
	let fun wrapId (i as {ctype=ct, ...}:Ast.id) = wrapExp (ct,Ast.Id i)
	    fun wrapExp'(ct,ce) = (#2 (wrapExp(ct,ce)))

	    fun declTid tid = 
		wrapDecl (Ast.ExternalDecl (Ast.TypeDecl {shadow=NONE,
							  tid=tid}))

	    fun makeMember (tid:Tid.uid) (n:string) (ct:Ast.ctype) :Ast.member =
		{name = SYM.member (tid,n), 
		 uid = Pid.new (), 
		 location = getLoc (),
		 ctype = ct,
		 kind = Ast.STRUCTmem } (* XXX - a bit brittle. *)

	    fun tidToNamedCtype tid =
		(case lookTid tid of
		     NONE => NONE
		   | SOME {name=_,ntype = n,location=_,global=_} => n)


	    fun cnvRecord ({errorMsg, name, view1, view2,
			   toView1, toView2,members} : HRecord) =
		let val _ = ifParseError errorMsg
		    val name = case name of NONE => Hid("record") | SOME n => n
		    val recSym = SYM.typedef name
		    val  v1Sym = SYM.typedef view1
		    val  v2Sym = SYM.typedef view2
		    val recTid = Tid.new ()
		    val  v1Tid = Tid.new ()
		    val  v2Tid = Tid.new ()

		    (* according to C spec redeclaration disallowed unless
		     types are compatible.  We don't check this.  Instead
		     insist they be unbound. *)
		    val _ = (unbound recSym;
			     unbound v1Sym;
			     unbound v2Sym)

		    (* FMS: threading decls/stmts through the recursion was a
		     nightmare.  This little bit of state does the trick,
		     *)
		    (* STATE *)
		    val (emitDecl ,getDecls, resetDecls ) = HL.genEmitter ()

		    val fields : (Ast.ctype * Ast.ctype option * 
				   string option) list = 
			let fun cnv ct' =
			    let 
                                val (ct,sc) = CTcnvType ct' 
			    in 
				case sc of
				    Ast.DEFAULT => ct
				  | _ => X.fail "No storage class may be specified for view fields."
			    end 
			    fun cnvField ct1 ct2o d =
				let val (ct1', n) = mungeTyDecr (cnv ct1,d)
				    fun munge' ct = 
					(#1 (mungeTyDecr (cnv ct,d)))
				in
				    (ct1',HL.ifSome munge' ct2o,n)
				end
			    fun loop [] = []
			      | loop ((ct1,ct2o,ds) :: tl) = 
				((List.map (cnvField ct1 ct2o) ds) @ 
				 (loop tl))
			in
			    loop members
			end

		    val coercionRequired =
			List.exists (fn (_,SOME _,_) =>true | _ =>false) fields
		    val coercionPresent =
			(case (toView1,toView2) of
			     (SOME _,SOME _) => true
			   | _  => false)
		    val _ = 
			if coercionRequired andalso not coercionPresent then
			    X.fail "Coercions required but not present."
			else ()

		    val (emitStmt1,getStmt1s', resetStmt1) = HL.genEmitter ()
		    val (emitStmt2,getStmt2s', resetStmt2) = HL.genEmitter ()
		    val emitStmt1 = 
			if coercionPresent then (fn x => ())
			else emitStmt1
		    val emitStmt2 =
			if coercionPresent then (fn x => ())
			else emitStmt2

		    fun getStmt1s () = 
			let val r = List.rev (getStmt1s' ())
			    val (s,d) = ListPair.unzip r
			in
			    (s,List.concat d)
			end
		    fun getStmt2s () = 
			let val r = List.rev (getStmt2s' ())
			    val (s,d) = ListPair.unzip r
			in
			    (s,List.concat d)
			end


		    val v1Var = Hid view1 (* Variables to be used in *)
		    val v2Var = Hid view2 (* coercion functions.     *)

		    (* SIDE-EFFECT: splitCT computes the default 
		     coercion using emitStmt1 and emitStmt2 *)
		    (* Automatic construction of record coercions. *)
		    fun splitCT (e1:PT.expression,
				 e2:PT.expression,
				 ct:ctype) : (ctype * ctype)
			=
			let fun defCo (to,from) = P.assignS(to,from)
			    fun default () = 
					 (emitStmt1(defCo(e1,e2),[]);
					  emitStmt2(defCo(e2,e1),[]);
					  (ct,ct))
			in
			    case ct of
				Ast.Array(SOME li,ct') =>
					 splitArray(e1,e2,li,ct')
			      | Ast.Array(NONE,ct') => 
				    (if coercionPresent then default ()
				     else X.fail "Cannot create coercions to arrays of unknown size.")
			      | Ast.TypeRef _ => splitTypeRef(e1,e2,ct)
			      | Ast.StructRef _ => splitStruct(e1,e2,ct)
			      | Ast.Qual(Ast.VOLATILE,ct') => splitCT(e1,e2,ct')
			      | Ast.Qual(Ast.CONST,_) =>
				    if coercionPresent then default ()
				    else X.fail "Cannot create coercions to const types"
			      | Ast.UnionRef _ => 
				    if coercionPresent then default ()
				    else X.fail "Cannot create coercions for union types."
			      | _ => default ()
			end
		    and splitStruct(e1,e2,ct) =
			let val (tid,members) = CTstructMembers  "2" ct
			    val name = 
				(case HL.deSome (lookTid tid) of
				    {name = SOME n, ... } => n
				  | _ => "")
				
			    val (tid1,tid2) = (Tid.new (), Tid.new ())

			    fun dot (e,m) = PT.Binop(PT.Dot,e,PT.Id m)

			    fun proc ((ct,m), (mem1s,mem2s)) =
				let val mn = (SYM.name (#name m))
				    val (ct1,ct2) = splitCT(dot(e1,mn),
							    dot(e2,mn),
							    ct)
				    val m1 = rebindMember (m,tid1,ct1)
				    val m2 = rebindMember (m,tid2,ct2)
				in
				    ((ct1,SOME m1,NONE) :: mem1s,
				     (ct2,SOME m2,NONE) :: mem2s)
				end
			    val (mem1s,mem2s) =
				List.foldr proc ([],[]) members
			    fun bind (tid,mems) = 
				bindTid(tid,{ name = SOME(Hid name),
					     ntype = SOME(B.Struct(tid, mems)),
					     location = getLoc(),
					     global = true}) (* XXX - global? *)
			in
			    bind (tid1, mem1s);
			    bind (tid2, mem2s);
			    emitDecl (declTid tid1);
			    emitDecl (declTid tid2);			    
			    (Ast.StructRef tid1, Ast.StructRef tid2)
			end
		    and splitTypeRef (e1,e2,ct) =
			(case CTrecord ct of
			     SOME r =>
				 let val {v1CT,v2CT,toView1,toView2,...} = r
				     fun assign (e1,e2) = 
					 PT.Expr (PT.Binop(PT.Assign,e1,e2))
				     val (ve1,vd1) = toView1 e2
				     val (ve2,vd2) = toView2 e1
				 in
				     emitStmt1 (assign (e1, ve1),vd1);
				     emitStmt2 (assign (e2, ve2),vd2);
				     (v1CT,v2CT)
				 end
			   | _ => splitCT (e1,
					   e2,
					   CTreduce ct))
		    and splitArray (e1,e2,(size,sizeExp),ct) =
			let val i = Hid "i"
			    val oldStmt1s = getStmt1s' ()
			    val oldStmt2s = getStmt2s' ()
			    val _ = resetStmt1 []
			    val _ = resetStmt2 []
			    fun sub e = PT.Binop(PT.Sub,e,PT.Id i)
			    val (ct1,ct2) = splitCT(sub e1,sub e2,ct)
			    val iDecl = P.varDeclS(P.int,i,PT.EmptyExpr)
			    fun for body =
				PT.Compound [iDecl,
					     PT.For(PT.Binop(PT.Assign,
							     PT.Id i,
							     P.zero),
						    PT.Binop(PT.Lt,
							     PT.Id i,
							     P.int32X size), (* XXX: should be sizeExp but Ast exp. *)
						    PT.Unop(PT.PostInc,
							    PT.Id i),
						    PT.Compound body)]
			    val (stmt1s,decl1s) = getStmt1s ()
			    val (stmt2s,decl2s) = getStmt2s ()
			    val for1 = for stmt1s
			    val for2 = for stmt2s
			in
			    resetStmt1 (oldStmt1s @ [(for1,decl1s)]);
			    resetStmt2 (oldStmt2s @ [(for2,decl2s)]);
			    (Ast.Array(SOME (size,sizeExp),ct1),
			     Ast.Array(SOME (size,sizeExp),ct2))
			end

		    (* Typecheck, and convert a user supplied coercion. *)
		    fun cnvCoercion (id:string,idCT:Ast.ctype,
				     stmt:PT.statement,stmtCT:Ast.ctype)
			:HI.coercion=
			(case stmt of 
			    PT.Expr exp =>
				let val expCT = stmtCT 
				    val _ = pushLocalEnv ()
				    val (argSym,argId) = localInitVar(id,idCT)
				    val (expCT',expAst) = cnvExp exp
				    val _ =
					if isAssignable(expCT',
							expCT,
							SOME expAst) then
					    ()
					else X.fail ("Coercion has type "^
						   (CTtoString expCT') ^
						   ". Expected type "  ^
						   (CTtoString expCT))
				    val _ = popLocalEnv ()
					
				    fun coercion pt =
					let fun subst p = HPTS.substExp (id,p,exp)
					in
					    if P.isExpPure pt then (subst pt,[])
					    else
						let val (t,tSym,tId,tDecl) = 
						    declTemp("temp",idCT)
						    val assignTemp =
							PT.Binop(PT.Assign,
								 PT.Id t,
								 pt)
						in
						    (PT.Binop(PT.Comma,
							      assignTemp,
							      subst (PT.Id t)),
						     [tDecl])
						end
					end
				in
				    coercion
				end
			  | PT.StatExt(H.HCompound _) =>
				let val _ = pushLocalEnv ()
				    val (argSym,argId) = localInitVar (id,idCT)
				    val _ = newFunction stmtCT 
				    val astS = cnvStmt stmt
				    val _ = popLocalEnv ()
				    (* FMS : could get name here. *)
				    val func = Hid "user_coercion"
				    val funcCT = Ast.Function (stmtCT,[idCT])
				    val funcSym = SYM.object func
				    val funcId = ASTglobalId(funcSym,funcCT)
				    val funcDecl =
					(Ast.FunctionDef (funcId,[argId],astS))
				    fun coercion p = 
					(PT.Call(PT.Id func,[p]),[])
				in
				    bindSym (funcSym,B.ID funcId);
				    emitDecl (wrapDecl funcDecl);
				    coercion
				end
			  | _ => X.bug "cnvCoercion: Unexpected statement.")

		    fun cnvField (ct1,SOME ct2,SOME n) = (n,ct1,ct2)
		      | cnvField (ct1,NONE,SOME n) = 		
			let fun dot e = PT.Binop(PT.Dot,e,PT.Id n)
			    val (ct1,ct2) = splitCT(dot (PT.Id v1Var),
						    dot (PT.Id v2Var),
						    ct1)
			in
			    (n,ct1,ct2)
			end 
		      | cnvField (_,_,NONE) = 
			X.fail "Fields in views with >1 field must be named."

		    (* When the userCoercion is required or present *)
		    fun userCoercion (ct1,ct2) =
			let val ((id1,s1),(id2,s2)) = 
			    case (toView1,toView2) of
				(SOME co1,SOME co2) => (co1,co2)
			      | _ => X.bug "Missing coercions."		       
			in
			    (cnvCoercion(id1,ct2,s1,ct1),
			     cnvCoercion(id2,ct1,s2,ct2))
			end

			
		    (* build a coercion function from a list of statements for
		     the sub-pieces. *)
		    fun buildFun (func:string,
				  to:string, toCT: ctype, 
				  from:string, fromCT: ctype,
				  body: PT.statement list,
				  decls: Ast.declaration list) =
			let val    toSym = SYM.object to
			    val  fromSym = SYM.object from
			    val     toId = (ASTlocalId (toSym, toCT))
			    val   fromId = (ASTlocalId (fromSym, fromCT))
			    val   toDecl = AST.VarDecl (toId,NONE)
			    val (_,toExp) = wrapId toId
				
			    val _ = pushLocalEnv ()
			    val _ = bindSym(toSym,B.ID toId)
			    val _ = bindSym(fromSym,B.ID fromId)
			    val _ = bindDecls decls
			    val astBody = List.map cnvStmt body
			    val _ = popLocalEnv ()
				
			    val bodyS : Ast.statement=
				let val return = 
				    wrapStmt(Ast.Return (SOME(toExp)))
				in
				    wrapStmt (Ast.Compound (toDecl :: decls, 
							    astBody@[return])) 
				end
			    
			    val funcCT = Ast.Function (toCT,[fromCT])
			    val funcSym = SYM.object func
			    val funcId = ASTglobalId(funcSym,funcCT)
			    val funcDecl =
				(Ast.FunctionDef (funcId,[fromId],bodyS))
			in
			    bindSym(funcSym,B.ID funcId);
			    emitDecl (wrapDecl funcDecl);
			    ()
			end

		    (* bind a view and its tid to a particular ctype *)
		    fun bindView (vSym:SYM.symbol) vTid vCT =
			ASTtypedefTid bindSym vSym vTid vCT

		    fun bindAllViews (ct1,ct2) = 
			(ASTtypedefTid bindSym recSym recTid Ast.Void;
			 bindView v1Sym v1Tid ct1;
			 bindView v2Sym v2Tid ct2; 
			 
			 (* CKIT is maintaining some state which we need to
			    flush here. *)
			let val ckitTids = resetTids ()
			in
			    List.app (fn tid => emitDecl (declTid tid)) ckitTids
			end;

			 emitDecl (declTid v1Tid);
			 emitDecl (declTid v2Tid);
			 ())


		    (* These are the typedef'd types that will be associated
		     * with the two views.  They are bound below after the
		     * types are known. 
		     *)
		    val (v1CT,v2CT) = (Ast.TypeRef v1Tid, Ast.TypeRef v2Tid)

		    (* Assume statements for coercion already emitted.  
		     * Create the actual coercion function. 
		     *)
		    fun genCoercion (v1CT,v2CT) : (HI.coercion*HI.coercion)=
			let val f1 = Hid view1
			    val f2 = Hid view2
			    val (stmt1s,decl1s) = 
				getStmt1s ()
			    val (stmt2s,decl2s) =
				getStmt2s ()
			    val _ = buildFun(f1,
					     v1Var,v1CT,
					     v2Var,v2CT,
					     stmt1s,
					     decl1s)
			    val _ = buildFun(f2,
					     v2Var,v2CT,
					     v1Var,v1CT,
					     stmt2s,
					     decl2s)
			    fun co1 p = 
				(PT.Call(PT.Id f1,[p]),[])
			    fun co2 p = 
				(PT.Call(PT.Id f2,[p]),[])
			in
			    (co1,co2)
			end

		    (* Compute the types and coercion functions for the two 
		     * views. 

		     FMS: In case you're wondering, fields = (view1 type, 
		     view2 type option, field name option) list
		     *)
		    val (co1,co2) =
			(case fields of
			     [] => X.fail "A view must have at least 1 field."
			   | [(ct1,SOME ct2,NONE)] => 
				 (bindAllViews (ct1,ct2);
				 userCoercion(v1CT,v2CT))
			   | [(ct1,     NONE,NONE)] =>
				 let val _ = resetStmt1 []
				     val _ = resetStmt2 [] 
				     val (ct1,ct2) = splitCT (PT.Id v1Var,
							      PT.Id v2Var,
							      ct1)
				 in
				     bindAllViews (ct1,ct2);
				     if coercionPresent then
					 userCoercion(v1CT,v2CT)
				     else (* For this corner case we may be 
					   slightly inefficient. Since this
					   always generates a function call.
					   *)
					 genCoercion(v1CT,v2CT)
				 end
			   | _ => 
				 let val _ = resetStmt1 []
				     val _ = resetStmt1 []
				     (* Actually computes the coercion! *)
				     val fs = List.map cnvField fields

				     (* Compute the types. *)
				     val tid1 = Tid.new ()
				     val tid2 = Tid.new ()
				     val mem1 = makeMember tid1
				     val mem2 = makeMember tid2
				     fun genMems (n,ct1,ct2) =
					 let val m1 = mem1 n ct1
					     val m2 = mem2 n ct2
					     fun bind (m:Ast.member) = 
						 bindSym (#name m, B.MEMBER m)
					 in
					     bind m1;
					     bind m2;
					     ((ct1,SOME m1,NONE),
					      (ct2,SOME m2,NONE))
					 end
				     val structMems = List.map genMems fs
				     val (v1Mems,v2Mems) = 
					 ListPair.unzip structMems
				     fun bind tid mems=
					 bindTid (tid,
						  {name = NONE,
						   ntype = 
						   SOME(B.Struct (tid,mems)),
						   location = getLoc (),
						   global = true}) (* XXX - global? *)
					 
				     val ct1 = Ast.StructRef tid1
				     val ct2 = Ast.StructRef tid2
					 
				     (* Bind all the types!! *)
				     val _ = bind tid1 v1Mems
				     val _ = bind tid2 v2Mems
				     val _ = emitDecl (declTid tid1)
				     val _ = emitDecl (declTid tid2)
				     val _ = bindAllViews (ct1,ct2)

				 in
				     if coercionPresent then 
					 userCoercion(v1CT,v2CT)
				     else 
					 genCoercion(v1CT,v2CT)
				 end)

		    val info = HI.Record {recName = name, 
					  v1Name = view1,
					  v1CT = v1CT,
					  v2Name = view2,
					  v2CT = v2CT,
					  toView1 = co1,
					  toView2 = co2
					  }
		    
		in
		    HS.addId(recTid,info);
		    HS.addId(v1Tid,info);
		    HS.addId(v2Tid,info);
		    List.rev (getDecls ())
		end

	    fun genParamTy(paramRepName : string,  params : (PT.ctype * PT.declarator) list):
		Ast.externalDecl list =
		    case params 
                        of [] => []
		      |  _ => (
			       let val members = List.map (fn(cty,decr)=> (cty,[(decr, PT.EmptyExpr)])) params
				   val paramStructSpec = PT.Struct{isStruct=true, 
								   tagOpt = SOME(paramRepName^"_s"),
								   members = members}
				   val paramCTstruct = P.makeCT [paramStructSpec]
				   val (paramAstCT,_) =  mungeTyDecr(#1 (CTcnvType(paramCTstruct)), PT.EmptyDecr) 
				   val paramTid = ASTtypedef (paramRepName, paramAstCT)
				   val ckitDecls = List.map declTid (resetTids())
				   val structDecls = ckitDecls @ [declTid paramTid]
			       in
				   structDecls
			       end (* end case *))

            fun isFunctionParam(params : (PT.ctype * string) list) = 
                let fun doOne (ct,s) = 
		      let val (act,_) = CTcnvType ct
		      in
			 case getFunction act
			 of NONE => false
			 | SOME fp => true     
		      end
		in
		    List.exists doOne params
		end

	    fun cnvPickle  {name,params,read,readArgs,write,writeArgs,typ} =
		let val initFunName   = Hid ("initDecl_"^name)     (* Name of init_declaration function *)
		    val initFunNoParamsName = Hid("initDecl_no_params_"^name) (* .. without parameters *)
		    val readFunName   = Hid("initNested_"^name)    (* Name of directory read function *)
		    val writeFunName  = Hid("write_"^name)         (* Name of write function *)

		    (* Variable names used in multiple generated functions. *)
		    val path      = Hid "path"
                    val readOnly  = Hid "readonly"
                    val mode      = Hid "mode"
		    val close     = Hid "close"
		    val paramSet  = Hid "paramSet"
		    val fileName  = Hid "fileName"
		    val topLevel  = Hid "topLevel"
		    val paramData = Hid "paramData"
		    val fileName  = Hid "fileName"
		    val data      = Hid "p"
		    val space     = Hid "space"

                    val paramRepName = Hid ("params_"^name)        (* Name of parameter table *)
		    val parameterized = not (List.length params = 0)
                    val paramCT = if parameterized
				  then P.typedef paramRepName           (* parameter table typedef *)
				  else P.void 
                    (* generate struct for parameter representation *)
		    val structDecls = genParamTy (paramRepName, params)

                    (* check and convert representation type of pickle *)
		    val (repCT,_) = CTcnvType typ

		    (* Conversion is not safe at the top-level! *)
		    val _ = pushLocalEnv ()
                    val typedParams = List.map (mungeTyParams ("pickle "^name)) params 
                    val unTypedParams = List.map PT.Id (#2(ListPair.unzip typedParams))
                    val hasFunctionParam = isFunctionParam typedParams

		    val (readCT,_) = cnvExp (PT.Id read)
		    val (writeCT,_) = cnvExp (PT.Id write)
                    (* get ast type list of argument types for read/write functions for checking. *)
                    val readArgCTs = List.map (#1 o cnvExp) readArgs
                    val writeArgCTs = List.map (#1 o cnvExp) writeArgs
		    val _ = popLocalEnv ()

		    val HRSioCT = #1 (CTcnvType (P.ptr HRS.ioCT))

		    fun checkFuncType ct actualCTs funDesc = 
			case getFunction ct of
			    SOME(retCT,argCTs) => (
				let val numArgs = List.length argCTs
				    val numAct = List.length actualCTs
				    val reqParams = [HRSioCT,Ast.Pointer repCT, CTchar]
				    val (setParamSupplied, matchCTs) = 
					if numArgs = numAct + 3 then (false, actualCTs @ reqParams)
					else if numArgs = numAct + 4 
					     then (true,  CTchar :: (actualCTs @ reqParams))
					     else (X.error (funDesc^
  					                " does not have the right number of parameters.");
						   (false, reqParams))
				in
				   if ListPair.all equalType (argCTs, matchCTs) andalso (CTisInt retCT)
                                      then setParamSupplied  (* everything is okay *)
                                      else let val matchCTstrings = List.map CTtoString matchCTs
                                               fun addCommas [] = ""
                                                 | addCommas [s] = s
                                                 | addCommas (s::ss) = s^", "^(addCommas ss)
                                               val matchCTstring = addCommas matchCTstrings
                                               in (X.error (funDesc^ 
                                                           " should have type: " ^ 
 					                   "int (*) ("^ matchCTstring ^ ")"); setParamSupplied)
					       end
				end (* end case *))
			  | NONE => 
				(X.error (funDesc^
					" did not have function type."); 
				 false)

		    val readSetParamSupplied =  checkFuncType readCT  readArgCTs  "Pickle read function"
		    val writeSetParamSupplied = checkFuncType writeCT writeArgCTs "Pickle write function"

                    val pickleCT = P.makeCT [PT.TypedefName name]

                    fun genReadWrapper name funName funArgs setSupplied = 
			let val userWrapperName = Hid("read_wrapper_"^name)
			    val filePtrX = P.arrowX (PT.Id data, PT.Id "file")
			    val setParam = if setSupplied then [PT.Id paramSet] else []
                            val paramList = List.map P.mkParam([(P.char, paramSet),
								(P.ptr paramCT, paramData),
							        (P.charPtr, path), 
							        (P.ptr HRS.pickleCT, data),
								(P.int, mode), (P.char, readOnly)])
                            val initPs = P.initParams (PT.Id paramSet,PT.Id paramData, 
						       typedParams, hasFunctionParam)
			    val openFileS = P.assignS(filePtrX,
						      HRS.openFileMode(PT.Id path, 
								       PT.Id mode, 
								       PT.Id readOnly))
                            val callUserReadX = PT.Call(PT.Id funName, 
						    setParam
						    @ funArgs 
						    @ [filePtrX,
						       PT.Cast(P.ptr typ,
							       P.arrowX(PT.Id data, PT.Id "data")),
						       PT.Id readOnly])
                            val bodyS = P.compoundS (initPs @ [openFileS, P.returnS callUserReadX])
			    val userWrapperPT =  P.mkFunctionEDecl(userWrapperName,
						      paramList, 
						      bodyS, 
						      P.ctToDT POR.int32CT)
			in
			     (userWrapperName, userWrapperPT)
			end
		    val (readWrapperName, readWrapperPT) = 
                           genReadWrapper name read readArgs readSetParamSupplied 

                    fun genWriteWrapper name funName funArgs setSupplied = 
			let val userWrapperName = Hid("write_wrapper_"^name)
			    val resultTemp = Hid("resultTemp")
			    val setParam = if setSupplied then [PT.Id paramSet] else []
                            val paramList = List.map P.mkParam([(P.char, paramSet),
								(P.ptr paramCT, paramData),
							        (P.charPtr, path), 
							        (P.ptr HRS.pickleCT, data),
								(P.char, close),
								(P.char, readOnly)])
			    val resDecl = P.varDeclS'(POR.int32CT, resultTemp)
                            val initPs = P.initParams (PT.Id paramSet,PT.Id paramData,
						       typedParams,hasFunctionParam)
			    val filePtrX = P.arrowX(PT.Id data, PT.Id "file")
                            val callUserS = P.assignS(PT.Id resultTemp,
					      PT.Call(PT.Id funName, 
						      setParam
						      @ funArgs 
						      @ [filePtrX,
							 PT.Cast(P.ptr typ,
								 P.arrowX(PT.Id data, PT.Id "data")),
							 PT.Id close]))
			    val closeS = PT.IfThen(PT.Id close, PT.Expr(HRS.closefile(filePtrX)))
                            val bodyS = P.compoundS ([resDecl ] @
						     initPs @ 
						     [callUserS, closeS, P.returnS (PT.Id resultTemp)])
			    val userWrapperPT =  P.mkFunctionEDecl(userWrapperName,
						      paramList, 
						      bodyS, 
						      P.ctToDT POR.int32CT)
			in
			     (userWrapperName, userWrapperPT)
			end
		    val (writeWrapperName, writeWrapperPT) = 
			   genWriteWrapper name write writeArgs writeSetParamSupplied 


		    val initFields = P.initParamStruct (PT.Id paramData) unTypedParams
		    fun genInitFun (name, typedParams, initFields, parameterized) = 
			let val paramList = List.map P.mkParam([(P.char,  paramSet)]
							       @ typedParams @
							       [(P.charPtr,  fileName),
								(POR.int32CT, mode    ),
								(POR.int32CT, readOnly),
								(P.char, topLevel)
								])
			    val (varDecls,paramX,paramSizeX,paramSetX) = 
				if parameterized 
				    then ([P.varDeclS'(paramCT,  paramData)],
					  P.addrX(PT.Id paramData),
					  P.sizeofX paramCT,
					  PT.Id paramSet)
				else ([], P.zero, P.zero, P.zero)
                            val openX = HRS.pickleOpen (PT.Id fileName, (PT.Id mode),
							(PT.Id readOnly), (PT.Id topLevel),
							(P.sizeofX typ), paramSizeX,
							paramSetX, paramX,
							(PT.Id readWrapperName), (PT.Id writeWrapperName),
							pickleCT)
			    val bodyS = P.compoundS (varDecls @ 
						     initFields @ 
                                                     [P.returnS openX])
			in
			    P.mkFunctionEDecl(name, paramList, bodyS, P.ctToDT pickleCT)
			end
		    val initFunPT = genInitFun(initFunName, typedParams, initFields, parameterized)
		    val (pVersionNameOpt, initFunNoParamsPT) = 
			if (not parameterized) orelse hasFunctionParam then (NONE,[])
			else (SOME initFunNoParamsName, 
			      [genInitFun(initFunNoParamsName, [],[],parameterized)])

		    fun genPickleReadFun ()  = 
			let val paramList = List.map P.mkParam
				[(P.char, paramSet),
				 (P.ptr paramCT, paramData),
				 (P.charPtr,  fileName),
				 (P.ptr (P.ptr typ),  space),
				 (POR.int32CT, mode),
				 (POR.int32CT, readOnly),
				 (P.char, topLevel)
				 ]
 
			    val initPs = P.initParams (PT.Id paramSet,PT.Id paramData, 
						       typedParams, hasFunctionParam)
                            val args = List.map (PT.Id o #2) typedParams
			    val pickleOpenX = PT.Call(PT.Id initFunName, 
						      [PT.Id paramSet]
                                                     @ args
						     @[PT.Id fileName,
						       PT.Id mode,
						       PT.Id readOnly,
						       PT.Id topLevel])
			    val bodyS = P.compoundS (initPs
						     @ [P.assignS(P.starX(PT.Id space),
								  pickleOpenX)])
			    val readFunPT = P.mkFunctionEDecl(readFunName,
							      paramList,bodyS, 
							      (P.makeDT [PT.Void]))
			in
			    readFunPT
			end

		    val readFunPT = genPickleReadFun ()
		    val pickleTid = ASTtypedef(name, Ast.Pointer repCT)
		    val info =
			{tid   = pickleTid,
			 typ   = pickleCT,
			 name  = name,
			 read  = PT.Id readWrapperName,
			 write = PT.Id writeWrapperName,
			 initFun = initFunName,
			 initFunNoParams = pVersionNameOpt,
			 readFun = readFunName,
			 writeFun = writeWrapperName,
			 paramTy = paramCT,
			 formalParams = typedParams}
		in
		    HS.addId(pickleTid, HI.Pickle info);
		    [declTid pickleTid]
                      @ structDecls
		      @ (cnvExternalDecl readWrapperPT)
		      @ (cnvExternalDecl writeWrapperPT)
		      @ (cnvExternalDecl initFunPT)
		      @ List.concat (List.map cnvExternalDecl initFunNoParamsPT)
   	              @ (cnvExternalDecl readFunPT) 
		end


	    fun cnvEventTy {name:string, 
			    elts: (PT.ctype * PT.declarator) list} =
		let fun doOneElt(ct,decr) 
		    : string * Ast.ctype * HI.EventElt * Tid.uid * 
		    (string * PT.ctype) * (string * PT.ctype) option= 
		    let val ct' = CTcnvType ct
			val (cty, nameOpt) = mungeTyDecr(#1 ct', decr)
			val fname = case nameOpt of 
			    NONE => (X.error "Munion field must have a name.\n";
				     "BOGUS")
			  | SOME n => n
			val vald = (P.mkEvtValue fname, CTtoPTct cty)
			val (ee,flagdOpt) = 
			    (case CThinfo cty of
				 SOME (HI.EventTy e) => (HI.Hier e, NONE)
			       | _ => (HI.Base,SOME (P.mkEvtFlag fname, P.char)))
			val tid = ASTtypedef(fname,Ast.Void)
		    in 
			(fname, cty, ee, tid, vald, flagdOpt)
		    end
		    val es = map doOneElt elts
		    fun agg ((fname,cty,ee,tid,vald,flagdOpt), 
			     (finfos,tids,valds,flagds)) = 
			(finfos @[(fname,cty,ee)], tid :: tids, vald :: valds,
			 case flagdOpt of 
			     NONE => flagds 
			   | SOME flagd => (flagd :: flagds))
		    val (finfos,tids,valds,flags) = 
			List.foldr agg ([],[],[],[]) es
		    val (CTeventTy,sc) = CTcnvType (P.struc(flags @ valds, 
							     NONE))
		   val tid = ASTtypedef(name,CTeventTy)
		   val eventInfo = (* compute offsets ??? *)
		       {name = name,
			elts = finfos}
		   val HIeventInfo = HI.EventTy eventInfo
		   val _ = List.app (fn tid => HS.addId(tid, HIeventInfo)) tids
                in
		    HS.addId(tid, HIeventInfo);
		    [(declTid tid)]
                end



	    fun cnvMap {errorMsg, name, params, keyType, split,rangeType, default,
			compress,decompress} =
		let val _ = ifParseError errorMsg
                    val initFunName = Hid("initDecl_"^name)  (*Name of initializing declaration function*)
                    val initFunNoParamsName = Hid("initDecl_no_params_"^name) (* ...with no supplied parameters *)
                    val readFunName = Hid("initNested_"^name) (* Name of directory read function *)
                    val writeFunName = Hid("write_"^name)     (* Name of directory write function *)
                    val defaultParamFunName = Hid("default_params_wrapper_"^name) (* Name of parameterized
										     default function wrapper.
										     Generated only if default
										     is a parameterized function*)
		    (* Variable names used in multiple generated functions. *)
		    val path      = Hid "path"
                    val readOnly  = Hid "readOnly"
                    val mode      = Hid "mode"
		    val close     = Hid "close"
		    val paramSet  = Hid "paramSet"
		    val fileName  = Hid "fileName"
		    val topLevel  = Hid "topLevel"
		    val paramData = Hid "paramData"
		    val fileName  = Hid "fileName"
		    val space     = Hid "space"
		    val size      = Hid "size"
		    val key       = Hid "key"
		    val value     = Hid "value"


                    val paramRepName = Hid ("params_"^name)             (* Name of map parameter table *)
		    val parameterized = not (List.length params = 0)    (* is map parameterized? *)
                    val paramCT = if parameterized                      (* parameter table typedef *)
				  then P.typedef paramRepName           (* parameter table if exists *)
				  else P.void                           (* void otherwise *)

		    val paramDecls = genParamTy (paramRepName, params)

                    val _ = pushLocalEnv()  (* put the type parameters in scope *)
                    (* generate struct for parameter representation *)
		    val typedParams : (PT.ctype * string ) list = 
			  List.map (mungeTyParams("map "^name)) params
		    val paramNames = List.map #2 typedParams
		    val unTypedParams : PT.expression list = List.map PT.Id (#2(ListPair.unzip typedParams))
                    val hasFunctionParam = isFunctionParam typedParams

		    val mapCT = CTfromTypeName HRS.mapTypeName

		    (* check that rangeType is valid. *)
		    val (rangeCT,rangeSC) = CTcnvType rangeType
		    val _ =
			if hasKnownStorageSize rangeCT then ()
			else X.fail ("Map value types must have a known size." ^
				   " Type " ^ (CTtoString rangeCT) ^
				   " does not have known size.")
		    val rangePtr = Ast.Pointer rangeCT


                    (* process compression portion of map declaration *)	
		    (* todo: generate wrapped versions of these functions *)
		    val (compType, compress, decompress) = 
			(case (compress,decompress) of
			    (NONE,NONE) => 
				let val (c,cdecl) = mapCompress (name, rangeType,paramCT,
								 genDefaultCompress(rangeType))
				    val (d,ddecl) = mapDecompress (name, rangeType, paramCT,
								   genDefaultDecompress(rangeType))
				    val edecls = ((cnvExternalDecl cdecl) @
						  (cnvExternalDecl ddecl))
				in
				    HS.addExtDecls edecls;
				    (Generic, PT.Id c,PT.Id d)
				end
			  | (NONE,SOME _) => 
				 (X.error "Decompression function requires a compression function.";
				  (Generic, PT.Id "bogus", PT.Id "bogus"))
			  | (SOME _, NONE) =>
				  (X.error "Compression function requires a decompression function.";
				   (Generic, PT.Id "bogus", PT.Id "bogus"))

			  | (SOME (cInfo),SOME (dInfo)) =>
				(* XXX - missing piece
				   Check that c,d are constant expressions. *)
				let val (compTypC, ce) = 
				             doFunCompress (name,rangeType, cInfo, typedParams,
							    paramCT,hasFunctionParam)
				    val (compTypD,de) = 
					     doFunDecompress (name,rangeType, dInfo,typedParams,
							      paramCT,hasFunctionParam)
				    val _ = if (compTypC = compTypD) then ()
					    else X.error("User-supplied compression functions "^
							 "must be either both stripe-level or "^
							 "both entry-level.")
				in
				    (compTypC,ce,de)
				end    
			)
                    (* Process key portion of map declaration *)
                    fun lookupRange ctype = (
                        case CThinfo ctype
                        of SOME(HI.Range r) => r
                        |  _ => (X.error ("Key type specification components "^
					  "must have range types.\n");
				 {name = "bogus", low = P.zero, high = P.intMax})
                        (* end CThinfo ctype case *))

		    fun chkNumeric (e,s) = 
			let val (ct,_) = cnvExp e
			in
			    if CTisNumber ct then ()
			    else X.error (s^" must have numeric type.\n")
			end
				  
                    fun processKeyInfo (keyType, split)  = 
                        let val (PTkeyCT, keyCT,numLevels,rngs,bS,sS) = 
			             case keyType 
			             of H.SKey ct => (
					   let val () = case split  
						        of NONE => ()
							|  SOME _ => X.error ("Map with structured "^
								  	      "key cannot have split clause.\n")
					       val () = case params 
						        of [] => ()
							| _ => X.error ("Map with structured "^
									"key (old-style maps) "^
									"cannot take parameters.\n")
					       val (keyCT,_) = CTcnvType ct
					       val () = if CTisStruct keyCT then () 
							else X.error ("Key type specification "^
							              "must be a structure.\n")
					       val (kTid,kMems) = CTstructMembers "3" keyCT
					       val numLevels = List.length kMems
					       val _ = if not ((numLevels = 2) orelse (numLevels = 3))
						       then X.error ("Key type specifications must "^
							             "have two or three ranges.\n")
						       else ()
					       val rngs = map (lookupRange o #1) kMems
					   in
					      (ct,keyCT,numLevels,rngs,P.zero,P.zero) 
							(* XXX what does HRS expect for block and split
							   sizes from maps with key type specifications?*)
					   end
					(* end SKey case *))
				     |  H.LKey(e1,e2) => (
					   let val () = chkNumeric(e1, "Key range limits")
					       val () = chkNumeric(e2, "Key range limits")
					       val (bS,sS) = 
						   (case split 
					            of SOME(bsX,ssX) => (
							 let val () = chkNumeric (bsX, "Split expressions")
							     val () = chkNumeric (ssX, "Split expressions")
							 in
							     (bsX,ssX)
							 end
							(* end SOME case *))
						    | NONE => (X.error ("Maps with limit key specifications "^
									  "must include a split clause.\n");
							       (P.intX 10000, P.intX 100))
						    (* end case split *))
                                               val (keyCT,_) = CTcnvType P.longlong
					       val rngs = [{name = name^"range", low = e1, high = e2}] 
					   in
					       (P.longlong, keyCT, 1, rngs, bS, sS)
					   end

					(* end LKey case *))

                            val keyName = Hid("key_desc")
			    fun initRange ({low,high,...}:HI.RangeInfo, i) = 
				let val arrayX = P.subX(P.dotX(PT.Id keyName, PT.Id "ranges"), P.intX i)
				    val lowX = P.dotX(arrayX, PT.Id "low")
				    val highX = P.dotX(arrayX, PT.Id "high")
				    val lowS = P.assignS(lowX,   low)
				    val highS = P.assignS(highX, high)
				in
				    [lowS,highS]
				end
			    fun initRanges [] i = [P.assignS(P.dotX(PT.Id keyName, PT.Id "levels"),
							     P.intX numLevels)]
                              | initRanges (r::rs) i = initRange (r, i) @ (initRanges rs (i + 1))

                            val keyDecl = P.varDeclS(HRS.keyDescCT, keyName, PT.EmptyExpr)
                            val keyInitSs = initRanges rngs 0
                            fun chkRangeForParams ({low,high,name}:HI.RangeInfo) = 
				let val low' = if HPTS.isFreeInExp(paramNames, low) 
				               then P.intX ~1 else low
				    val high' = if HPTS.isFreeInExp(paramNames, high) 
				               then P.intX ~1 else high
				in
				    {low=low', high=high', name=name}
				end
			    val rngsNS = List.map chkRangeForParams rngs
			    val keyInitSsNS = initRanges rngsNS 0
                        in
                           (keyName,keyDecl,keyInitSs, keyInitSsNS, keyCT,PTkeyCT,bS,sS)
                        end

                    val (keyName,keyDecl,keyInitSs, keyInitSsNS, keyCT,PTkeyCT,blockSplitX,stripeSplitX) 
			       = processKeyInfo (keyType, split)


                    (* Process default portion of map declaration *)
                    datatype DefType = Constant | Function | ParamFunction

		    (* FMS: This produces poor error messages but we 
		        have no hook for converting initializers. *)
                    fun chkExp (e, s) = (case e
                         of PT.MARKexpression(loc,e') => chkExp (e', s)
                         |  PT.InitList _ => ()
                         |  _ => let val (ct,_) = cnvExp e
				       in
					   if isAssignable(rangeCT, ct,NONE) then ()
					   else X.error (s ct)
				       end 
		         (* end case *))

                    fun errMsg ct = "Type of default ("^
			             (CTtoString ct) ^
				     ") is not compatible with declared "^
				     "value type of map ("^
				     (CTtoString rangeCT)^
				     ").\n"

		    fun genConstantDefault e =
			let val h = Hid ("default_"^name)
			    val () = chkExp(e,errMsg)
			    val decl = P.varDeclS (rangeType,h,e)
			in
			    ((h,Constant), [decl], [], HPTS.isFreeInExp(paramNames, e))
			end

		    fun genFunDefault(e,retCT,keyArgCT,rangeArgCT) =
			let val h = 
			    let fun getId (PT.Id h) = h
				  | getId (PT.MARKexpression(_,e)) = getId e
				  | getId _ = (X.error "Map default function is not an identifier.\n";
					       "bogus")
			    in
				getId e
			    end
			    val _ = 
				if equalType (retCT,CTint) then ()
				else X.error ("Map default function should return void, not type " ^ 
					   (CTtoString retCT))
			    val _ = 
				if isAssignable(rangePtr,rangeArgCT,NONE) 
				    then ()
				else X.error ("Map default function expected range argument of type "^
					   (CTtoString rangePtr)^
					   ". Found type "^
					   (CTtoString rangeArgCT)^
					   ".")
			    val _ =
				if isAssignable(keyCT,keyArgCT,NONE) then
				    ()
				else X.error ("Map default function argument was "^
					   (CTtoString keyArgCT)^
					   ".  Expected type "^
					   (CTtoString keyCT))
			in
			    ((h,Function),[],[], HPTS.isFreeInExp(paramNames,e))
			end

		    fun chkParamDefault (f,args) = 
			let val (funCT,_) = cnvExp (PT.Id f) (* okay because pushed local scope above*)
			in
                           (case getFunction funCT
			    of SOME(retCT,formalCTs) => (
				let val numFormalArgs = List.length formalCTs
				    val numActualArgs = List.length args
				in
				    if numFormalArgs = numActualArgs + 3 (* paramSet, key, value*)
				    then let val actCTs = List.map (#1 o cnvExp) args
					     val reqArgCTs = [CTchar] @ actCTs @ [keyCT,rangePtr]
					 in
					    if ListPair.all equalType (formalCTs, reqArgCTs)
					       andalso (CTisInt retCT)
				            then ()
				            else (X.error ("Parameterized function default "^f^
							   " application is type incorrect.\n"))
					 end
				    else (X.error ("Parameterized function default "^f^
						   " passed wrong number of parameters.\n"))
				end)
                            | NONE => (X.error ("Expected function type for "^f^";"^
					       "found: "^(CTtoString funCT)))
			   (*end case getFunction *))
			end
		    fun genParamWrapper(f,args) = 
			let val paramList = List.map P.mkParam([(P.char, paramSet),
								(P.ptr paramCT, paramData),
								(PTkeyCT, key),
								(P.ptr rangeType, value)])
                            val initPs = P.initParams (PT.Id paramSet, PT.Id paramData, 
				                      typedParams, hasFunctionParam)
                            val callUserDefaultX = 
				PT.Call(PT.Id f, 
					[(PT.Id paramSet)]
					@ args
					@ [PT.Id key, PT.Id value])
                            val bodyS = P.compoundS (initPs @ [PT.Return callUserDefaultX])
			    val userWrapperPT =  P.mkFunctionEDecl(defaultParamFunName,
						      paramList, 
						      bodyS, 
						      P.ctToDT P.int)
			in
			    [userWrapperPT]
			end

                    fun genParamDefault(f,args) = 
			let val () = chkParamDefault(f,args)
                            val Edecls = genParamWrapper(f,args)
			    val depends = HPTS.isFreeInExp(paramNames, PT.Id f)
			in
			    ((defaultParamFunName, ParamFunction),[],Edecls,depends)
			end

		    val ((defName,defType),defaultDecls, defaultWrapperPTs, defDependsOnParam) = 
			case default of
			    NONE => genConstantDefault (PT.InitList [P.zero])
			  | SOME (H.NonParam e) =>
			       (case e 
				of PT.MARKexpression(loc,PT.InitList e') => genConstantDefault e
                                | _ => (let val (ct,_) = cnvExp e
					in 
					    case (getFunction ct) 
						of NONE => genConstantDefault e					 
					      |  SOME (retCT,[kCT,rCT]) => genFunDefault(e,retCT,kCT,rCT)
					      |  SOME (retCT,argCTs) =>
						    (X.error ("Map default function should take 2 "^
							      "arguments -- (key,value *)."); 
						     (("bogus",Function),[],[],false))
					end)
				(* end case *))
			   | SOME (H.Param (f,es)) =>  genParamDefault(f,es)

                    val _ = popLocalEnv()  (* Remove type parameters from scope. *)


		    fun genInitFun (fname, withParams, typedParams,
				    defaultName, defType, keyInitSs, compType) =
			let val paramList = List.map P.mkParam([(P.char,  paramSet)]
							       @ typedParams @
				                               [(P.charPtr,  fileName),
							        (POR.int32CT, mode    ),
							        (POR.int32CT, readOnly),
							        (POR.int32CT, topLevel)
								])
			    val (varDecls,paramX,paramSizeX, paramSetX) = 
				if parameterized 
				    then ([P.varDeclS'(paramCT,  paramData)],
					  P.addrX(PT.Id paramData),
					  P.sizeofX paramCT,
					  PT.Id paramSet) 
				else ([], P.zero, P.zero, P.zero)
			    val initParamFields = if withParams 
						  then P.initParamStruct (PT.Id paramData) unTypedParams 
						  else []
			    val defDescName = Hid ("default_description_"^name)
			    val defDescId = PT.Id defDescName
			    val defDescDecls = [P.varDeclS(HRS.mapDefaultCT,
							 defDescName, PT.EmptyExpr)]
				
			    val defaultDecls = if (not withParams) andalso defDependsOnParam
						   then [P.varDeclS(rangeType,defaultName,
								    PT.InitList[P.zero])]
					       else defaultDecls
			    val defDescInitS = case defType
				               of Function => HRS.setDefaultFunc defDescId defaultName
					       |  Constant => HRS.setDefaultConst 
						                (defDescId,defaultName,
								 if (not withParams) andalso 
								    defDependsOnParam then
								     HRS.constDefNS else HRS.constDefS)
					       |  ParamFunction => HRS.setDefaultParamFunc 
								      defDescId defaultName

			    val compDescName = Hid("compress_description_"^name)
			    val compDescId = PT.Id compDescName
			    val compDescDecls = [P.varDeclS(HRS.mapCompressCT,
							 compDescName, PT.EmptyExpr)]
			    val compDescType = 
				case compType
				of Generic =>    HRS.mapGenericCompress
				|  UserEntry =>  HRS.mapUserEntryCompress
				|  UserStripe => HRS.mapUserStripeCompress
			    val compDescInitS = HRS.setCompression (compDescId, compress,
								     decompress, compDescType)


			    val blockSplitX = if (not withParams) andalso 
				                 HPTS.isFreeInExp(paramNames, blockSplitX)
					      then P.intX ~1 else blockSplitX
                            val stripeSplitX = if (not withParams) andalso
					         HPTS.isFreeInExp(paramNames, stripeSplitX)
				              then P.intX ~1 else stripeSplitX

			    val openX = HRS.mapOpenLL(PT.Id fileName, P.sizeofX rangeType, defDescId,
				                      PT.Id readOnly, PT.Id mode, PT.Id topLevel,
						      PT.Id keyName, blockSplitX, stripeSplitX, 
						      compDescId,
						      paramSetX, paramSizeX, paramX)
			    val openS = P.returnS openX
			    val bodyS = P.compoundS (varDecls @ defaultDecls @ defDescDecls 
						     @ compDescDecls @ [keyDecl]
						     @ initParamFields @ keyInitSs 
						     @ defDescInitS @ compDescInitS @ [openS])
			    val initFunPT = [P.mkFunctionEDecl(fname, paramList,
								    bodyS, P.ctToDT HRS.mapCT)]
			in
			    initFunPT
			end

		    val openFunPT = genInitFun (initFunName, true, typedParams,
						defName, defType, keyInitSs, compType)

		    val  (pVersionNameOpt, noParamsPT) = 
			if (not parameterized) orelse hasFunctionParam then (NONE,[])
			else (SOME initFunNoParamsName, 
			      genInitFun(initFunNoParamsName, false, [], defName, 
					 defType, keyInitSsNS, compType))


                    fun genReadFun () = 
			let val paramList = List.map P.mkParam
				[(P.char, paramSet),
				 (P.ptr paramCT, paramData),
				 (P.charPtr,  fileName),
				 (P.ptr HRS.mapCT,  space),
				 (POR.int32CT, mode),
				 (POR.int32CT, readOnly),
				 (P.char, topLevel)
				 ]

			    val initPs = P.initParams (PT.Id paramSet, PT.Id paramData,
						       typedParams, hasFunctionParam)
			    val args = List.map (PT.Id o #2) typedParams
			    val mapOpenPX = PT.Call(PT.Id initFunName, 
						   [PT.Id paramSet]
						   @ args @
						   [ PT.Id fileName,
						    PT.Id mode, PT.Id readOnly, PT.Id topLevel])
                            val mapOpenNPX = PT.Call(PT.Id initFunNoParamsName, 
						   [PT.Id paramSet,
						    PT.Id fileName,
						    PT.Id mode, PT.Id readOnly, PT.Id topLevel])
                            fun mapOpen openX = P.assignS(P.starX(PT.Id space),openX)
			    val openCondS = if hasFunctionParam then
				               mapOpen mapOpenPX
					    else PT.IfThenElse(PT.Id paramSet,
							       mapOpen mapOpenPX,
							       mapOpen mapOpenNPX)
			    val bodyS = if parameterized
					    then P.compoundS(initPs @ [openCondS])
					else P.compoundS(initPs @ [mapOpen mapOpenPX])
			    val readFunPT = P.mkFunctionEDecl(readFunName,
							      paramList,bodyS, 
							      (P.makeDT [PT.Void]))
			in
			    readFunPT
			end
		    val readFunPT = genReadFun()

(*Generate multi-union with single label hid(mapName_e) with 
key type of map (long long) return list of decls  *)
                   (* Generate default event type *)
		    val eventName = Hid("event_"^name^"_e")
                    val fieldName = Hid("event_"^name)
		    val evtPT = {name=eventName,
				 elts=[(P.longlong, PT.PointerDecr(PT.VarDecr fieldName ))]}
		    val edecls = cnvEventTy evtPT
                 
		    (* if SOME(default)
		     check default has appropriate type. *)
		    (* XXX - missing piece *)
		    val mapTid = ASTtypedef(name,mapCT)
		    val mapInfo = 
			{ tid = mapTid,
			 keyCT = keyCT,
			 PTkeyCT = PTkeyCT,
			 PTrangeCT = rangeType,
			 rangeCT = rangeCT,
			 initFun = initFunName,
			 initFunNoParams = pVersionNameOpt,
			 readFun = readFunName,
			 compress = compress,
			 decompress = decompress,
			 paramTy = paramCT,
			 formalParams = typedParams,
			 defEvent = fieldName}	
		in
		    HS.addId(mapTid, HI.Map mapInfo);
		    (paramDecls @
                     [(declTid mapTid)] 
		      @ edecls
		      @ List.concat (List.map cnvExternalDecl defaultWrapperPTs)
		      @ List.concat (List.map cnvExternalDecl openFunPT) 
		      @ List.concat (List.map cnvExternalDecl noParamsPT) 
		      @ (cnvExternalDecl readFunPT))
		end


	    fun readZeroVal (fpId, control) = 
		let val numRead = Hid "numRead"
		    in
			PT.Compound[
				    P.varDeclS(P.int,numRead,PT.EmptyExpr),
				    P.assignS(PT.Id numRead,
					      PT.Call(PT.Id "sfscanf",
						      [fpId, PT.String control])),
				    PT.IfThen(P.notX(
					      P.orX(
					        P.eqX((P.intX ~1), PT.Id numRead),
					        P.eqX(P.zero, PT.Id numRead))),
					      HRS.errorS("Error in reading data"^
						       " from directory.\n") )
				    ]
		end

            fun readLine (fpId) = readZeroVal(fpId,"\n")


	    fun readOneVal (fpId, control, spaceId) = 
		let val numRead = Hid "numRead"
		    in
			PT.Compound[
				    P.varDeclS(P.int,numRead,PT.EmptyExpr),
				    P.assignS(PT.Id numRead,
					      PT.Call(PT.Id "sfscanf",
						      [fpId, PT.String control,
						       P.addrX(spaceId)])),
				    PT.IfThen(P.neqX((P.intX 1), PT.Id numRead),
					      HRS.errorS("Error in reading data"^
						       " from directory.\n") )
				    ]
		end

	    fun readOneValSpace (fpId, control, spaceId) = 
		let val numRead = Hid "numRead"
		    val sp = Hid "sp"
		    val sp' = PT.Id sp
		    in
			PT.Compound[
				    P.varDeclS(P.int,numRead,PT.EmptyExpr),
				    P.varDeclS(P.char,sp,PT.EmptyExpr),
				    P.assignS(PT.Id numRead,
					      PT.Call(PT.Id "sfscanf",
						      [fpId, PT.String control,
						       P.addrX(spaceId),
						       P.addrX(sp')])),
				    PT.IfThen(P.neqX((P.intX 2), PT.Id numRead),
					      HRS.errorS("Error in reading data"^
						       " from directory.\n") )
				    ]
		end


	    fun readType (ct,rootX,fp) : PT.statement  = 
		let  
                    fun readOne rootX PTty control= 
			let val tempi = Hid "i"
			    in PT.Compound
			            [P.varDeclS(PTty,tempi,PT.EmptyExpr),
				     readOneVal(PT.Id fp, control, PT.Id tempi),
				     readLine (PT.Id fp),
				     P.assignS(rootX, PT.Id tempi)]
			end

		    fun readString rootX =
			let val size   = Hid "size"
			    val index  = Hid "index"
			    val theStr = Hid "theStr"

			    val size'   = PT.Id size
			    val index'  = PT.Id index
			    val theStr' = PT.Id theStr
			    in PT.Compound
				[P.varDeclS(P.int,size,PT.EmptyExpr),
				 P.varDeclS(P.int,index,PT.EmptyExpr),
				 P.varDeclS(P.ptr P.char,theStr,PT.EmptyExpr),
				 readOneValSpace(PT.Id fp, "[%d]:%c", size'), 
				 P.assignS(rootX, 
					   CL.malloc(P.plusX(size',P.intX 1))),
				 P.assignS(theStr', rootX),
				 PT.For(P.assignX(index', P.zero),
					P.ltX(index', size'),
					P.postIncX index',
					readOneVal(PT.Id fp, "%c",
						   P.subX(theStr',
							  index'))),
				 P.assignS(P.subX(theStr', size'), P.zero)]
			end

                    fun readStruct (ct, rootX) = 
			let val cml : (ctype * Ast.member) list = #2(CTstructMembers "4" ct)
			    fun doField (ct, am:Ast.member) =
				let val fieldName = SYM.name(#name(am))
				    val newRootX = 
					P.dotX(rootX, PT.Id(fieldName))
				in 
				    PT.Compound
				    [readZeroVal(PT.Id fp, (fieldName^": ")), 
				     readType(ct,newRootX,fp)]
				end
			in
				PT.Compound ([readLine(PT.Id fp)] @
					     (map doField cml) @
                                             [readLine(PT.Id fp)])

			end

                    fun readArray (ct, rootX) =
			let val (sizeopt,cty) = HL.deSome (CTarrayInfo ct) 
			    val sizeX = 
				case sizeopt
				    of NONE => 
					(X.error ("Arrays in directories"^
						  "must have known size.\n"); 
					 P.zero)
				  | SOME (n,exp) => PT.IntConst n
			    val index = Hid "i"
			    val temp = Hid "temp"
			    val index' = PT.Id index
			    val loopBody =
				[readOneValSpace(PT.Id fp, "[%d]:%c", PT.Id temp),
				 readType(cty, 
					  P.subX(rootX, index'),
					  fp)]
			    val loopS = 
				PT.For(P.assignX(index', P.zero),
				       P.ltX    (index',  sizeX),
				       P.postIncX index',
				       PT.Compound loopBody) 
				      
			in
			    PT.Compound[P.varDeclS(P.int, index, PT.EmptyExpr),
					P.varDeclS(P.int, temp, PT.EmptyExpr),
					readLine (PT.Id fp),
					loopS,
					readLine (PT.Id fp)]
			end

                    fun readCType ct =   
                       if (CTisFixedSizeArray ct) then (readArray(ct,rootX))
		       else if (CTisString ct) then (readString rootX)
                       else if (CTisStruct ct) then (readStruct(ct,rootX))
		       else 
			   (case CTtoFormat ct of 
				SOME(f) => readOne rootX (CTtoPTct ct) f
			      | NONE => 
				    (X.error((CTtoString ct) ^ 
					     " unimplemented in directories.");
				     PT.Compound []))
		in 
		    case CThinfo ct of
			SOME(HI.Record _) => readCType ct
		      |	SOME(_) =>  
			    (X.error ("Hancock type "^(CTtoString ct) ^
				      " can't be nested in directories."); 
			     PT.Compound [])
		      | NONE => readCType ct

		end
            

	      

            fun buildReadFun (filename, ct : ctype, (ptCT,ptDecr,defaultX)) = 
		let val funName = Hid ("read_"^filename)
		    val (fixTy,argFn) = 
			case CTarrayInfo ct
			    of NONE => (Ast.Pointer ct, fn argX => P.addrX argX)
			    |  SOME(sizeopt, ct') => (Ast.Pointer ct', fn argX => argX)

		    val fieldCT = CTtoPTct (fixTy)
                    val paramList =  List.map P.mkParam
			             [( P.char,    "paramSet"),
                                      ( fieldCT,   "paramData"),
				      ( P.charPtr, "filename"),
                                      ( fieldCT,   "space"),
                                      ( P.int,     "size"),
                                      ( P.int,     "mode"),
                                      ( P.int,     "readonly"),
				      ( P.int,     "topLevel")]
                    val retTy = P.makeDT [PT.Void]

		    val rootX = PT.Id "space"
                    val rootX = if P.isArrayDecr ptDecr then rootX else P.starX rootX
                    val initS = if CTisString ct
			        then PT.Compound [P.assignS(P.starX(PT.Id "space"), CL.malloc(P.intX 1)),
						  P.assignS(P.subX(P.starX(PT.Id "space"), P.zero), P.zero)]
				else PT.Expr(PT.Call (
					           PT.Id "bzero", [PT.Cast(P.voidPtr, PT.Id "space"), 
							           PT.Id "size"]))
		    val derefFn = if CTisString ct then fn argX => P.starX argX else fn id => id
                    val copyS = PT.Expr(PT.Call (PT.Id "memcpy", 
						 [derefFn(PT.Id "space"), 
						  PT.Cast(P.voidPtr ,derefFn(PT.Id "paramData")), 
						  PT.Id "size"]))

		    val condAllocS = if CTisString ct
			            then [P.assignS(P.starX(PT.Id "space"), CL.malloc(PT.Id "size"))]
				    else []					   
		    val ifNotSetS = PT.IfThen(PT.Id "paramSet", PT.Compound (condAllocS @ [copyS]))
              
 
                    val bodyS = PT.Compound(
                                [PT.Decl(
                                   PT.Declaration(P.makeDT [PT.TypedefName "Sfio_t"],
                                                  [(PT.PointerDecr(PT.VarDecr "fp"),
						    PT.EmptyExpr)])),
				 PT.IfThenElse(P.notX (PT.Id "paramData"),
					       initS, ifNotSetS),       
				 PT.IfThen(P.neqX(PT.Id "mode", HRS.new),
					   PT.Compound(
					      [P.assignS(PT.Id "fp",
							 HRS.openFileMode(PT.Id "filename", 
									  PT.Id "mode", PT.Id "readonly")),
					      (readType (ct, rootX, "fp")),
					      PT.Expr(HRS.closefile(PT.Id "fp"))]))
                                ])
		    val funAstL = cnvExternalDecl 
                                  (P.mkFunctionEDecl(funName, paramList, bodyS, retTy))
		in
                   (PT.Id funName, argFn, funAstL)
		end


            fun getReadFun (filename,ct,pt) =
		case (getHTyInfo ct) of
		    (SOME(HI.Map m),params) => (PT.Id (#readFun(m)),
						fn argX => P.addrX argX,
						[])
		  | (SOME(HI.Pickle p),params) => (PT.Id (#readFun(p)), 
						   fn argX => P.addrX argX,
						   [])
		  | (SOME(HI.Dir d),params) => (PT.Id (#readFun(d)), 
						fn argX => P.addrX argX,
						[])
		  | _ => buildReadFun(filename,ct,pt)



	    fun writeType (ct,rootX,fp) : PT.statement  = 
		let 
		    fun writeOne rootX control =
			PT.Compound
		           [PT.Expr(PT.Call(PT.Id "sfprintf",
					    [PT.Id fp, PT.String control,
					     rootX])),
			    CL.writeLine fp]

 		    fun writeString rootX = PT.Compound
		                   [CL.writeArg(fp,"[%d]: ", CL.strlen rootX),
				    PT.Expr(PT.Call(PT.Id "sfprintf",
						    [PT.Id fp, PT.String "%s",
						     rootX])),
				    CL.writeLine fp]
 
                   fun writeStruct (ct, rootX) = 
			let val cml : (ctype * Ast.member) list = #2(CTstructMembers "5" ct)
			    fun doField (ct, am:Ast.member) =
				let val fieldName = SYM.name(#name(am))
				    val newRootX = P.dotX(rootX, PT.Id(fieldName))
				in 
				    PT.Compound[
				       CL.writeStr(fp,(fieldName^": ")),
				       writeType(ct,newRootX,fp)]

				end
			in
				PT.Compound ([CL.writeLine fp]@
					     (map doField cml) @
				             [CL.writeLine fp])
			end

                    fun writeArray (ct, rootX) =
			let val (sizeopt,cty) = HL.deSome (CTarrayInfo ct) 
			    val sizeX = case sizeopt
				        of NONE => (X.error ("Arrays in directories"^
                                                           "must have known size.\n"); P.zero)
				        | SOME (n,exp) => PT.IntConst n
			    val index = Hid "i"
			in
			    PT.Compound[P.varDeclS(P.int,index,PT.EmptyExpr),
					CL.writeLine fp,
					PT.For(P.assignX(PT.Id index, P.zero),
					       P.ltX(PT.Id index, sizeX),
					       P.postIncX(PT.Id index),
					       PT.Compound[
					         CL.writeArg(fp,"[%d]: ", PT.Id index),
                                                 writeType(cty, P.subX(rootX,
								    PT.Id index ),fp)]),
					CL.writeLine fp]
			end

  		     fun writeCType ct = 	
			    if (CTisFixedSizeArray ct) then (writeArray(ct,rootX))
		       else if (CTisString ct) then (writeString rootX)
                       else if (CTisStruct ct) then (writeStruct(ct,rootX))
		       else (case CTtoFormat ct of
				 SOME(f) => (writeOne rootX f)
			       | NONE =>
				     (X.error ((CTtoString ct) ^ 
					       " not implemented in directories.\n"); 
				      PT.Compound []))
		in
		    case CThinfo ct of
			SOME(HI.Record _ ) => writeCType ct
		      |	SOME(_) =>			    
			    (X.error ("Hancock type "^(CTtoString ct) ^
				      " can't be nested in directories.\n"); 
			     PT.Compound [])
		      | NONE => writeCType ct

		end 

            fun buildWriteFun (filename, ct : ctype, (ptCT,ptDecr,_)) = 
		let val funName = Hid ("write_"^filename)
		    val (fixTy,argFn) = case CTarrayInfo ct
			                of NONE => (Ast.Pointer ct, fn argX => P.addrX argX)
				        |  SOME(sizeopt, ct') => (Ast.Pointer ct', fn argX => argX)

		    val fieldCT = CTtoPTct (fixTy)
                    val paramList =  List.map P.mkParam
			             [( P.char, "paramSet"),
                                      ( fieldCT, "paramData"),
				      ( P.charPtr,"filename"),
                                      ( fieldCT, "space"),
                                      ( P.int,   "close"),
				      ( P.int,   "readonly")]
                    val retTy = P.makeDT [PT.Void]
		    val rootX = PT.Id "space"
		    val rootX = 
			if P.isArrayDecr ptDecr then rootX else P.starX rootX 
		    val ctype = 
			if P.isPtrDecr ptDecr then P.ptr(P.ptr ptCT) 
			else (P.ptr ptCT)
                    val bodyS = PT.Compound
                                [PT.Decl(
                                   PT.Declaration(P.makeDT [PT.TypedefName "Sfio_t"],
                                                  [(PT.PointerDecr(PT.VarDecr "fp"),
						    PT.EmptyExpr)])),
				 P.assignS(PT.Id "fp", 
					   HRS.openfile(PT.Id "filename", PT.String "w")),
				 (writeType (ct, rootX,"fp")),
				 PT.Expr(HRS.closefile(PT.Id "fp"))
				 ]
                    val bodyS' = PT.Compound[PT.IfThen(P.notX(PT.Id "readonly"), bodyS)]
		    val funAstL = cnvExternalDecl 
                                  (P.mkFunctionEDecl(funName, paramList, bodyS', retTy))
		in
                   (PT.Id funName, funAstL, false)
		end


            fun getWriteFun (filename,ct,pt) =
		case getHTyInfo ct of
		    (SOME(HI.Map m), params) => (PT.Id HRS.mapClose, [], true)
		  | (SOME(HI.Pickle p), params) => (PT.Id HRS.pickleClose, [], true)
		  | (SOME(HI.Dir d), params) => (PT.Id HRS.dirClose, [],true)
		  | _ => buildWriteFun(filename,ct,pt)
          

            fun copyCTypeS(ct,toX,fromX) = 
		let fun copyString(ct,toX,fromX) = 
		        let val size = Hid "size"
			in
			    PT.Compound[
                                P.varDeclS(P.int,size,PT.EmptyExpr),
				P.assignS(PT.Id size, CL.strlen(fromX)),
				P.assignS(toX, CL.malloc(P.plusX(PT.Id size, P.intX 1))),
				PT.Expr(CL.strcpy(toX,fromX))]
			end
		    fun copyArrayS(ct,toX,fromX) = 
		      let val (sizeopt,cty) = HL.deSome (CTarrayInfo ct) 
                          val sizeX = case sizeopt
				        of NONE => (X.error ("Arrays in directories"^
                                                           "must have known size.\n"); P.zero)
				        | SOME (n,exp) => PT.IntConst n
                          val index = Hid "i"
                      in
			    PT.Compound[P.varDeclS(P.int,index,PT.EmptyExpr),
					PT.For(P.assignX(PT.Id index, P.zero),
					       P.ltX(PT.Id index, sizeX),
					       P.postIncX(PT.Id index),
					       copyCTypeS(cty, P.subX(toX,PT.Id index),
                                                               P.subX(fromX,PT.Id index)))]
		      end
		    fun copyStructS (ct, toX,fromX) = 
			let val cml : (ctype * Ast.member) list = #2(CTstructMembers "6" ct)
			    fun doField (ct, am:Ast.member) =
				let val newFromX = P.dotX(fromX, PT.Id(SYM.name(#name(am))))
				    val newToX = P.dotX(toX, PT.Id(SYM.name(#name(am))))
				in 
				       copyCTypeS(ct,newToX,newFromX)
				end
			in
				PT.Compound (map doField cml) 
			end
		in
                    if (CTisFixedSizeArray ct) then copyArrayS(ct,toX,fromX)
		    else if (CTisString ct) then copyString(ct,toX,fromX)
                    else if (CTisStruct ct) then copyStructS(ct,toX,fromX)
                    else P.assignS(toX, fromX)
		end

            fun getCopyS(ct, toX, fromX) =
                 case (getHTyInfo ct)
                 of (NONE,_) => copyCTypeS(ct,toX, fromX)
                 | (SOME(HI.Map m), params) => PT.Expr(HRS.mapCopy(toX, fromX))
		 | (SOME(HI.Pickle p), params) => PT.Expr(HRS.pickleCopy(toX, fromX))
		 | (SOME(HI.Dir d), params) => PT.Expr(PT.Call (PT.Id (#copyFun d), [ toX, fromX]))
	         | (SOME(HI.Record _), params) => copyCTypeS(ct,toX,fromX)  (*XXX: nested hancock types ?*)
                 | _ => (X.error ("copy unimplemented for " ^ 
				  (CTtoString ct) ^ 
				  ".\n"); 
			 PT.Compound [])

	    fun cnvDirectory {name, members, params} =
               let  val repName = Hid name^"_Rep"
                    val repStructName = Hid name^"_Rep_s"
		    val paramRepName = Hid name^"_Params"
		    val extParamName = Hid name^"_ExtParam"
                    val extParamStructName = Hid name^"_ExtParam_s"

                    val dirTypeCT = P.typedef name               (* user directory type *)
		    val dirRepTypeCT = P.typedef repName         (* underlying representation *)

                    val initFunName   = Hid ("initDecl_"^name)     (* Name of init_declaration function *)
		    val initFunNoParamsName = Hid("initDecl_no_params_"^name) (* .. without parameters *)
		    val initNestedFunName  = Hid("initNested_"^name)    (* Name of init function for within dirs*)
		    val readFunName   = Hid("read_"^name)          (* Name of read function *)
		    val writeFunName  = Hid("write_"^name)         (* Name of write function *)
                    val copyFunName   = Hid ("copy_"^name)         (* Name of copy function *)

		    (* Variable names used in multiple generated functions. *)
		    val path      = Hid "path"
                    val mode      = Hid "mode"
                    val readOnly  = Hid "readonly"
		    val topLevel  = Hid "topLevel"
                    val close     = Hid "close"
		    val paramSet  = Hid "paramSet"
		    val paramData = Hid "paramData"
		    val fileNames = Hid "fileNames"
		    val extParamTable = Hid "extParamTable"
		    val rep       = Hid "dir"
		    val extData   = Hid "extData" 

                    (* extract parameter information from params *)
		    val typedParams = List.map (mungeTyParams ("directory "^name)) params 
		    val unTypedParamNames = #2(ListPair.unzip typedParams)
                    val unTypedParams = List.map PT.Id (unTypedParamNames)
                    val hasFunctionParam = isFunctionParam typedParams

                    (* Construct user directory type and underlying representation 
                     * Underlying representation is a C struct corresponding to the
                     * directory declaration.  dirRepTypeCT is a typedef for this struct
                     * User type is a pointer to this struct.  dirTypeCT is a typedef 
                     * for this pointer type. *)
 
		    val members = map (fn {declaration} => declaration) members
                    val PTmembers = List.concat (map (fn (ct,del) => 
					              (map (fn (d,e) => (ct,d,e)) del)) 
						 members)
		    val structMembers = map (fn (ct, del) => 
					     (ct, (map (fn (d,e) => (d,PT.EmptyExpr)) del)))
			                members
                    val dirPT = P.makeCT [PT.Struct { isStruct = true,
						     tagOpt = SOME (repStructName),
						     members = structMembers}]
		    (* the following will convert parameter declarators within
		     * the directory type, generating decls added to HS.externalDecls
		     * an imperative stucture.  Must pop and return at end *)
                    val (dirCT,dirCTsc) = CTcnvType dirPT 
                    val paramDecls = HS.popExtDecls()

		    val dirTid = ASTtypedef(name,Ast.Pointer dirCT)
		    val dirRepTid = ASTtypedef(repName, dirCT)

                    val cmel = #2(CTstructMembers "7" dirCT)
		    val numCompsX = P.intX (List.length cmel)
		    val cmpl = ListPair.zip (cmel : (ctype * Ast.member) list, 
					     PTmembers : (PT.ctype
							  * PT.declarator 
							  * PT.expression) list)

		    (* Check whether or not any fields require a field in param table 
                     * Also check if any components have function parameters, and hence
                     * make parameters required. (so don't generate no_param init function). *)
                    fun chkField ((ct,am:Ast.member),
				  pt:(PT.ctype*PT.declarator*exp)) : ((string * PT.ctype option) * bool) =
			let val fname = SYM.name(#name(am))
			    fun chkNPFunc NONE = true  (* does compoenent have a non_param init function?*)
                              | chkNPFunc (SOME _) = false  
			    fun doNonParamHTy (ct,PT.MARKexpression(loc,e)) =
				    doNonParamHTy(ct,e)
                              | doNonParamHTy (ct, PT.EmptyExpr) = (NONE, false)
                              | doNonParamHTy (ct, _) = (SOME (CTtoPTct ct), false)
			    val (tyOpt,hasFunctionParam) = case CThinfo ct
				of SOME(HI.Param{basety,exprs}) => (
				   case CThinfo basety
                                   of SOME(HI.Pickle pi) => (SOME (#paramTy pi), chkNPFunc (#initFunNoParams pi))
				   |  SOME(HI.Dir di) => (SOME(#paramTy di), chkNPFunc (#initFunNoParams di))
				   |  SOME(HI.Map mi) => (SOME(#paramTy mi), chkNPFunc (#initFunNoParams mi))
				   |  _ => (NONE, false)
				   (* end case basety *))
                                | _ => doNonParamHTy(ct,#3 pt)
			in 
			    ((fname,tyOpt),hasFunctionParam)
			end

                    val fieldInfo = List.map chkField cmpl
                    val (fieldInfo, hasFparam) = ListPair.unzip fieldInfo
                    val doNotGenNonParam = List.exists (fn x=>x) hasFparam

		    (* Generate Extended parameter table type *)
		    fun extract [] = []
		      | extract ((n,c)::rest) = case c of NONE => extract rest 
			                              | SOME ct => (n,ct)::(extract rest)
		    val prunedTys = extract fieldInfo
                    val extExists = not (List.length prunedTys = 0)
		    val (extParamNameCT, extMems, extDecls) = (* external parameter table type *)
			if extExists
                         then 
                          let val extParamTablePT = P.struc (prunedTys, SOME(extParamStructName))
			      val (extParamCT,_) = CTcnvType extParamTablePT
			      val extParamTid = ASTtypedef(extParamName, extParamCT)
			      val extParamNameCT = P.typedef extParamName
			  in
			      (extParamNameCT,  [(extParamNameCT, PT.VarDecr extData)],
			       [declTid extParamTid])
			  end
		         else
		              (P.void, [],[])

		    val paramFields = params @ extMems
		    val parameterized = not(List.length paramFields = 0)
                    val paramCT = if parameterized
			          then P.typedef paramRepName         (* parameter table *)
				  else P.void 
		    val structDecls = genParamTy (paramRepName, params @ extMems) 


                    (* Process each field in directory, extracting necessary information. *)
                    fun doIthField (((ct,am:Ast.member),pt), i, dict) : 
			((string * PT.ctype option) list (* field name, type of parameter *) *
			 Ast.externalDecl list           (* ast rep for read/write functions*) * 
			 PT.statement list               (* parameter type decls for read function *) * 
			 PT.statement list               (* parameter type decls for write function *) * 
                         PT.statement list               (* read code snippets *) *
                         PT.statement list               (* init code snippets *) *
                         PT.statement list               (* write code snippets *) *
                         (string * PT.expression) list   (* dictionary: prior fields to rt values*) ) = 
			let val fname = SYM.name(#name(am))              (* field name *)
			    val (baseTy, tyParams) = getHTyInfo ct
                            val fieldParamData = fname^"_paramData"
			    val fieldParamSet  = fname^"_paramSet"
			    val initExtParamS = P.assignS(P.arrowX(PT.Id extParamTable,PT.Id fname),
						           PT.Id fieldParamData)
			    fun calcParamSet exps = 
				let val dep =  List.exists (P.dependsOn unTypedParamNames) exps
				in
				    if dep then PT.Id paramSet else P.trueX
				end
                            fun doSubst e = 
				let fun doit ((s,arg), exp) : PT.expression = HPTS.substExp(s,arg,exp)
				in
                                   List.foldr doit e dict
				end

			    fun initParams (paramTy, formalParams : (PT.ctype * string) list, exprs) =
				let val _ = 
				           if not(List.length exprs = List.length formalParams)
			                   then X.error ("Number of formal params does not "^
					                 "match number of actuals.\n")
					   else ()
				    val paramNames = List.map #2 formalParams
				    val exprs = List.map doSubst exprs
				    val paramInitX = calcParamSet exprs
				in
				    (SOME paramTy, 
				     [P.varDeclS(paramTy,  fieldParamData, PT.EmptyExpr),
				      P.varDeclS(P.char,  fieldParamSet, paramInitX)],
				     [],
				     (P.initParamStruct2 (PT.Id fieldParamData) (List.map #2 formalParams) exprs)
				     @ [initExtParamS],
				     true,
				     []) (* size not used for Hancock types. *)
				end

                            fun getSize (ct,paramTy,exp) = 
				if CTisString ct
				    then  
					case exp
					of PT.EmptyExpr => [P.intX 0]
                                        | _ => [P.plusX(CL.strlen(exp), P.intX 1)]
				else [P.sizeofX paramTy]

			    fun doDefault (ct,PT.MARKexpression(loc,e)) :
                                 (cty option * stmt list * stmt list * stmt list * bool * exp list)
			      = doDefault (ct,e)
                              | doDefault (ct,exp) = 
				  let val paramTy = CTtoPTct ct
                                      val size = case CThinfo ct
					         of NONE => getSize (ct,paramTy,exp)
						 |  SOME(_) => [] (* size parameter used for only c types *)
				      val exp = doSubst exp
				      val (paramTyOpt, defaultRX, defaultWX, hasParams, paramSetX) = 
				      case exp
				      of PT.EmptyExpr => (NONE, PT.EmptyExpr, PT.EmptyExpr, false, P.falseX)
                                      |  e as PT.InitList l => 
					      (* This produces poor error messages, but we 
					       * have no hook for determining the type of initLists,
					       * so it is the best we can do. *)
					      let 
(* This type check causes some necessary typedef definitions
   to be omitted from output code.  In particular, typedefs
   that encode type parameter applications.  Have asked ckit
   people for a function that type checks initialization code. 
   Any type erros will be reported later. KSF*)
(*			  val _ = cnvExternalDecl(P.varEDecl(paramTy,fname^"default",e))  *)
					      in
  						  (SOME paramTy, e, 
						   PT.EmptyExpr,
						   true, calcParamSet [exp])
					      end
				      |  e => let val (dct,dsc) = cnvExp e
					          val _ = if isAssignable(ct,dct,NONE) then ()
						          else X.error("Type of default for field "^
								       fname ^" ("^(CTtoString dct)^") "^
								       "does not match the declared "^
								       "type of the field ("^
								       (CTtoString ct) ^ ").")
					      in 
						  (SOME paramTy, e,
						   P.arrowX(PT.Id extParamTable, PT.Id fname),
						  true, calcParamSet [exp])
					      end
				      val (paramDecls,initParams) = 
					  if hasParams
					  then ([P.varDeclS(paramTy,fieldParamData,defaultRX)],
						[copyCTypeS(ct, P.arrowX(PT.Id extParamTable, PT.Id fname),
							    PT.Id fieldParamData)])
					  else ([],[])
				  in
				      (paramTyOpt,
				       paramDecls @ [P.varDeclS(P.char, fieldParamSet, paramSetX)],
				       [P.varDeclS(P.char, fieldParamSet, paramSetX)],
				       initParams,
				       hasParams,
				       size)
				  end

                            val (paramTyOpt, readVarDecls, writeVarDecls, 
				 readInitFields, hasParams, sizeXs) = 
				case CThinfo ct
				of SOME (HI.Param{basety,exprs}) => (
				      case CThinfo basety 
                                      of SOME(HI.Pickle pi) =>
					   initParams (#paramTy pi, #formalParams pi, exprs)
                                      |  SOME(HI.Dir di) =>
					   initParams (#paramTy di, #formalParams di, exprs)
				      |  SOME(HI.Map mi) =>
					   initParams (#paramTy mi, #formalParams mi, exprs)
                                      | _ => (NONE, [],[],[],false, [])  (* size not used for H types*)
			      (* end case basety *))
				| _ => 	(doDefault (ct,#3 pt))

                            val (freadX, argFn, readAstl) = getReadFun(fname,ct,pt)
                            val paramX = if hasParams then argFn(PT.Id fieldParamData) else P.zero
			    val freadCall = PT.Expr (PT.Call(freadX, 
							    [PT.Id (fieldParamSet), 
							     paramX,
							     P.subX(PT.Id fileNames, P.intX i),
							     argFn(P.arrowX(PT.Id rep, PT.Id fname))]
							     @ sizeXs @
							     [ PT.Id mode, PT.Id readOnly, P.falseX]))
                            val (fwriteX, writeAstl,hclose) = getWriteFun(fname,ct,pt)
			    val paramX = if hasParams 
					 then argFn(P.arrowX(PT.Id extParamTable,PT.Id fname)) 
					 else P.zero
			    val fwriteCall = if hclose then
						 PT.Expr(PT.Call(fwriteX,
								 [P.arrowX(PT.Id rep, PT.Id fname)]))		
					     else
						 PT.Expr (PT.Call(fwriteX, 
							    [PT.Id (fieldParamSet), 
							     paramX,
							     P.subX(PT.Id fileNames, P.intX i),
							     argFn(P.arrowX(PT.Id rep, PT.Id fname))]
							     @ [PT.Id close, PT.Id readOnly]))
                        in ([(fname, paramTyOpt)],
			    readAstl @ writeAstl, 
			    readVarDecls, writeVarDecls, readInitFields @[freadCall],
                            [ P.assignS(P.subX(PT.Id fileNames,P.intX i), PT.String fname)],
			    [fwriteCall],
			    (fname, P.arrowX(PT.Id rep, PT.Id fname))::dict)
			end
                    fun doFields [] i dict = ([],[],[],[],[],[],[],[])
                      | doFields (f::fs) i dict = 
			let val res as (fnty,asts,readVarDecls,writeVarDecls,readPTs,initPTs,writePTs,dict') = 
			               doIthField(f,i,dict)
			    in
				HL.appendOct res (doFields fs (i+1) dict')
			end
                    val (fieldInfo, astl,readVarDecls, writeVarDecls, 
			 readPtl, initPtl, writePtl,dict) = doFields cmpl 0 []
		    val writePtl = List.rev writePtl
		    val fieldNames = List.map (fn (n,c)=> n) fieldInfo



                    (* Generate functions:
                         initDecl -- called at initializing declaration site.
                         initDecl_no_params -- called at initializing declaration
			                       site when programmer did not supply parameters.
                         initNested -- called by containing directories to initialize
			               this directory when it is nested.
                         read     -- called to initialize pieces of directory
                                     by invoking read functions for constituents.
				     passed to HRSdirReg.
                         write    -- called to flush data for constituents to disk
			             by invoking constituent write functions.
				     passed to HRSdirReg.
                         copy     -- used to copy directory by calling
			             copy functions for constituents.
                     *)			  

		    (* Generate function to process initializing declaration.
 		     * Prototype of function:
		     * dir_d initDecl_dirName (char paramSet, 
		     *                         <list of parameters to type, ie, int dinit, char cinit,>
		     *                         char *path, char readonly, char mode) 
		     * This function calls runtime system directory registration function at end.
		     *)

                    fun genInitFun() = 
			let val paramList = List.map P.mkParam ([(P.char, paramSet)]
						    @ typedParams
						    @ [(P.charPtr      , path),
						       (P.char         , mode),
						       (P.char         , readOnly),
						       (P.char         , topLevel)])
			    val fNameDecl = P.varDeclS(P.array(numCompsX,P.charPtr), 
						       fileNames, PT.EmptyExpr)
			    val paramDecls = if parameterized 
					    then [(P.varDeclS'(paramCT, paramData))]
					    else []
			    val varDecls = fNameDecl :: paramDecls 
			    val initFields = P.initParamStruct (PT.Id paramData) unTypedParams
			    val paramX = if parameterized then P.addrX (PT.Id paramData) else P.zero
                            val paramSetX = if parameterized then PT.Id paramSet else P.zero
			    val extSizeX = if extExists then P.sizeofX extParamNameCT else P.zero

			    val regX = 
				PT.Cast(dirTypeCT,
					HRS.dirReg(PT.Id path, 
						   PT.Id fileNames,
						   numCompsX, 
						   P.sizeofX dirRepTypeCT,
						   PT.Id mode, PT.Id readOnly, PT.Id topLevel,
						   paramSetX, paramX,
						   extSizeX,
						   PT.Id readFunName,
						   PT.Id writeFunName))
			    val bodyS = PT.Compound (varDecls @ initFields @ initPtl @ [P.returnS regX])
			in
			    P.mkFunctionEDecl(initFunName, 
					      paramList, 
					      bodyS, 
					      P.ctToDT dirTypeCT) 
			end
		    val initFunPT = genInitFun()

                    fun genNestedInitFun() = 
			let 
			    val paramList = List.map P.mkParam ([(P.char, paramSet)]
						    @ [(P.ptr paramCT  , paramData),
						       (P.charPtr      , path),
						       (P.ptr dirTypeCT , rep),
						       (P.char         , mode),
						       (P.char         , readOnly),
						       (P.char         , topLevel)])
			    val fNameDecl = P.varDeclS(P.array(numCompsX,P.charPtr), 
						       fileNames, PT.EmptyExpr)
			    val varDecls = [fNameDecl]
			    val extSizeX = if extExists then P.sizeofX extParamNameCT else P.zero
			    val regX = 
				PT.Cast(dirTypeCT,
					HRS.dirReg(PT.Id path, 
						   PT.Id fileNames,
						   numCompsX, 
						   P.sizeofX dirRepTypeCT,
						   PT.Id mode, PT.Id readOnly, PT.Id topLevel,
						   PT.Id paramSet, PT.Id paramData,
						   extSizeX,
						   PT.Id readFunName,
						   PT.Id writeFunName))
			    val initS = P.assignS(P.starX(PT.Id rep), regX)

			    val bodyS = PT.Compound (varDecls @ initPtl @ [initS])
			in
			    P.mkFunctionEDecl(initNestedFunName, 
					      paramList, 
					      bodyS, 
					      P.makeDT [PT.Void])
			end
		    val nestedInitFunPT = genNestedInitFun()

                    fun genInitFunNoParams () =
                        let val paramList = List.map P.mkParam ([(P.char, paramSet)]
								@ [(P.charPtr      , path),
								   (P.char         , mode),
								   (P.char         , readOnly),
								   (P.char         , topLevel)])
			    val varDecls = P.declareParams typedParams
			    val callX = PT.Call(PT.Id initFunName, 
						([PT.Id paramSet]
						 @ unTypedParams
						 @ [PT.Id path, PT.Id mode, PT.Id readOnly, PT.Id topLevel]))
			    val callS = P.returnS callX
			    val bodyS = P.compoundS(varDecls @[callS])
			    val initFunNoParamsPT = P.mkFunctionEDecl(initFunNoParamsName, paramList,
								    bodyS, P.ctToDT dirTypeCT)
                        in
			    initFunNoParamsPT
			end
		    val (pVersionNameOpt, noParamsFunPT) = 
			if (not parameterized) orelse hasFunctionParam orelse doNotGenNonParam
			then (NONE,[]) else 
			     (SOME initFunNoParamsName, [genInitFunNoParams ()])

		    fun genReadFun () =
			let val paramList = 
				[(P.makeDT [PT.Char],            PT.VarDecr paramSet),
				 (P.makeDT [PT.Pointer paramCT], PT.VarDecr paramData),
 				 (P.makeDT [PT.Pointer extParamNameCT], PT.VarDecr extParamTable),
 				 (P.ctToDT dirTypeCT,            PT.VarDecr rep),
				 (P.ctToDT P.charPtrPtr,         PT.VarDecr fileNames),
				 (P.makeDT [PT.Int],             PT.VarDecr mode),
				 (P.makeDT [PT.Char],            PT.VarDecr readOnly)]
			    val initPs = P.initParams (PT.Id paramSet,PT.Id paramData, 
						       typedParams, hasFunctionParam)
                            val args = List.map (PT.Id o #2) typedParams
			    val bodyS = P.compoundS (initPs @ [ P.compoundS (readVarDecls @ readPtl)])
						     
			    val readFunPT = P.mkFunctionEDecl(readFunName,
							      paramList,bodyS, 
							      (P.makeDT [PT.Void]))
			in
			    readFunPT
			end
		    val readFunPT = genReadFun ()


		    fun genWriteFun () =
			let val paramList = 
				[(P.makeDT [PT.Char],            PT.VarDecr paramSet),
 				 (P.makeDT [PT.Pointer extParamNameCT], PT.VarDecr extParamTable),
 				 (P.ctToDT dirTypeCT,            PT.VarDecr rep),
				 (P.ctToDT P.charPtrPtr,         PT.VarDecr fileNames),
				 (P.makeDT [PT.Char],            PT.VarDecr close),
				 (P.makeDT [PT.Char],            PT.VarDecr readOnly)]
			    val bodyS = P.compoundS (writeVarDecls @ writePtl)
			    val writeFunPT = P.mkFunctionEDecl(writeFunName,
							       paramList,bodyS, 
							       (P.makeDT [PT.Void]))
			in
			    writeFunPT
			end
		    val writeFunPT = genWriteFun ()

                    val copyParamList = 
			List.map P.mkParam [(dirTypeCT, "to"),
					    (dirTypeCT, "from")]
                    fun copyField (ct,am:Ast.member) : PT.statement  = 
			let val fname = SYM.name(#name(am))
                        in 
                           getCopyS(ct,P.arrowX(PT.Id "to" ,PT.Id fname),
				       P.arrowX(PT.Id "from" ,PT.Id fname))
			end
                    val copyBodyS = PT.Compound (map copyField cmel)
                    val PTretDT = P.makeDT[PT.Void]
                    val copyFunPT = P.mkFunctionEDecl(copyFunName, 
						      copyParamList, 
						      copyBodyS, 
						      PTretDT) 
                    val hInfo =
                        {name = name,
                         tid = dirTid,
                         PTtypeCT = dirTypeCT,
                         initFun = initFunName,
			 initFunNoParams = pVersionNameOpt,
			 readFun = initNestedFunName,
			 writeFun = writeFunName,
                         copyFun = copyFunName,
			 paramTy = paramCT,
			 formalParams = typedParams
			 }
                in
                   HS.addId(dirTid, HI.Dir hInfo);
		   paramDecls
                   @ [declTid dirTid, declTid dirRepTid]
                   @ extDecls
		   @ structDecls
                   @ astl
		   @ (cnvExternalDecl readFunPT)  
		   @ (cnvExternalDecl writeFunPT)  
		   @ (cnvExternalDecl initFunPT) 
		   @ (cnvExternalDecl nestedInitFunPT)
		   @ List.concat (List.map cnvExternalDecl noParamsFunPT) 
		   @ (cnvExternalDecl copyFunPT)
                end




            fun cnvRange {name, low, high} = 
                let fun chkBound (v,expTy,expAst) =
		    let val _ = 
			if isAssignable(CTint, expTy, SOME expAst) then ()
			else X.error ("Range components for "^name^
				    " must be compatible with type int.\n")
		    in
			v
		    end
		
                    fun getVal e = case evalExpr e of
			(SOME v,ct,expAst,false) => chkBound(v,ct,expAst)
		      | (_,_,_,_) => (X.error  ("Range components for "^name^
					      " must be constant.\n"); IntInf.fromInt 0)
			    
                    fun chkBounds(e1,e2) =
			let val v1 = getVal e1
                            val v2 = getVal e2
			in
			    if (IntInf.>(v1,v2)) then 
				X.error ("Upper bound of range "^
				       name^
				       " must be greater than lower bound.\n")
			    else ()
			end


                    val _ = chkBounds(low,high)
		    val tid = ASTtypedef(name,CTint)
                    val hInfo =
                        {name = name,
                         low  = low,
                         high = high }
                in
                   HS.addId(tid, HI.Range hInfo);
                   [(declTid tid)]
                end

	    fun cnvStream {name, params, logicalType, physicalType, transform, transformArgs} =
		(* XXX - we should be checking the type of tranform here. *)
		let
		    val initFunName   = Hid ("initDecl_"^name)     (* Name of init_declaration function *)
		    val initFunNoParamsName = Hid("initDecl_no_params_"^name) (* .. without parameters *)

                    val paramRepName = Hid ("params_"^name)        (* Name of parameter table *)
		    val paramSet  = Hid "paramSet"
		    val paramData = Hid "paramData"
		    val fileName  = Hid "fileName"
                    val readOnly  = Hid "readonly"
                    val mode      = Hid "mode"
		    val topLevel  = Hid "topLevel"

		    val parameterized = not (List.length params = 0)
                    val paramCT = if parameterized
				  then P.typedef paramRepName           (* parameter table typedef *)
				  else P.void 
                    (* generate struct for parameter representation *)
		    val structDecls = genParamTy (paramRepName, params)
		    (* Conversion is not safe at the top-level! *)
		    val _ = pushLocalEnv ()
                    val typedParams = List.map (mungeTyParams ("pickle "^name)) params 
                    val unTypedParams = List.map PT.Id (#2(ListPair.unzip typedParams))
                    val hasFunctionParam = isFunctionParam typedParams
		    val (transCT,_) = cnvExp (PT.Id transform)
                    val transformArgCTs = List.map (#1 o cnvExp) transformArgs
                    val transformArgPTcts = List.map CTtoPTct transformArgCTs
		    val transformParamed = not (List.length transformArgs = 0)

		    val _ = popLocalEnv ()

		    val (lCT,lSC) = (CTcnvType logicalType)
		    val (pCT,pSC) = (CTcnvType physicalType)
                    val isIO =    equalType(pCT,  #1(CTcnvType HRS.ioCT))
		    val isGenerative =   equalType(pCT, Ast.Void)
		    val _ = 
			(case (lSC,pSC) of
			     (Ast.DEFAULT,Ast.DEFAULT) => ()
			   | _ => X.error "No storage class may be specified for stream types.")
			 
		    val _ =
			if isIO orelse isGenerative then ()
			else if hasKnownStorageSize pCT then ()
			else 
			    X.error ("Type " ^
				   (CTtoString pCT) ^
				   " in stream declaration must have " ^
				   "a statically known size.")

		    val _ =
			if hasKnownStorageSize lCT then ()
			else 
			    X.error ("Type " ^
				   (CTtoString lCT) ^
				   " in stream declaration should have " ^
				   "a statically fixed size.")

		    val physTys = (* check type of transform *)
			let val paramArgs = if transformParamed then 
			                       P.char :: transformArgPTcts
			                    else []
			    val ePhysTys = if isGenerative then []
				           else [P.ptr physicalType]
			    val (expectCT,_) =
			    CTcnvType (P.ptr (P.func P.int 
					      (paramArgs @ ePhysTys @ [P.ptr logicalType])))
			in
			   (if isAssignable(expectCT,transCT,NONE) then ()
			    else X.error ("Type of transformation function (" ^
					(CTtoString transCT) ^
					") does not match expected type " ^
					(CTtoString expectCT) ^
					".");
			    ePhysTys)
			end

                    fun genTransWrapper (sName, fName, fArgs, isParamd) = 
			let val wrapperName = Hid("trans_wrapper_"^sName)
			    val physRep = Hid("physRep")
			    val logRep = Hid("logRep")
			    val (physFParams,physAParams) = 
				case physTys of [] => ([],[])
			      | [PTty] => ([(PTty, physRep)], [PT.Id physRep])
                              | _ =>  X.bug "Expected one element list."
                            val paramList = List.map P.mkParam([(P.char, paramSet),
								(P.ptr paramCT, paramData)]
							       @ physFParams @
                                                               [(P.ptr logicalType, logRep)])
                            val initPs = P.initParams (PT.Id paramSet, PT.Id paramData, 
						       typedParams, hasFunctionParam)
			    val setParam = if isParamd then [PT.Id paramSet] else []
                            val callUserTransX = PT.Call(PT.Id fName, 
						    setParam @ fArgs @ physAParams
						    @ [PT.Id logRep])
                            val bodyS = P.compoundS (initPs @ [P.returnS callUserTransX])
			    val transWrapperPT =  P.mkFunctionEDecl(wrapperName,
						      paramList, bodyS, P.ctToDT POR.int32CT)
			in
			    (wrapperName, transWrapperPT)
			end
		    val (transWrapperName, transWrapperPT) = 
			genTransWrapper(name, transform, transformArgs, transformParamed)
									     

                    val streamPtCT = P.makeCT [PT.TypedefName name]
		    val initFields = P.initParamStruct (PT.Id paramData) unTypedParams
		    fun genInitFun (name, typedParams, initFields, parameterized) = 
			let val paramList = List.map P.mkParam([(P.char,  paramSet)]
							       @ typedParams @
							       [(P.charPtr,  fileName),
								(POR.int32CT, mode    ),
								(POR.int32CT, readOnly),
								(P.char, topLevel)])
			    val (varDecls,paramX,paramSizeX,paramSetX) = 
				if parameterized 
				    then ([P.varDeclS'(paramCT,  paramData)],
					  P.addrX(PT.Id paramData),
					  P.sizeofX paramCT,
					  PT.Id paramSet)
				else ([], P.zero, P.zero, P.zero)
			    val streamTypX = if isIO 
					     then HRS.isIO
					     else if isGenerative
						  then HRS.isGenerative
						  else HRS.isBin
				         
			    val phsSizeX = if isIO orelse isGenerative
					       then P.zero else P.sizeofX physicalType
                            val openX = HRS.streamReg (PT.Id fileName, 
						        paramSizeX,
							paramSetX, paramX,
							streamTypX,
							phsSizeX,
							P.sizeofX logicalType,
							PT.Id transWrapperName)
			    val bodyS = P.compoundS (varDecls @ 
						     initFields @ 
                                                     [P.returnS openX])
			in
			    P.mkFunctionEDecl(name, paramList, bodyS, P.ctToDT streamPtCT)
			end
		    val initFunPT = genInitFun(initFunName, typedParams, initFields, parameterized)
		    val (pVersionNameOpt, initFunNoParamsPT) = 
			if (not parameterized) orelse hasFunctionParam then (NONE,[])
			else (SOME initFunNoParamsName, 
			      [genInitFun(initFunNoParamsName, [],[],parameterized)])

		    val streamCT = CTfromTypeName HRS.streamTypeName

                   (* Generate default event type *)
		    val eventName = Hid("event_"^name^"_e")
                    val fieldName = Hid("event_"^name)
		    val evtPT = {name=eventName,
				 elts=[(logicalType, PT.PointerDecr (PT.VarDecr fieldName) )]}

		    val edecls = cnvEventTy evtPT
		    val tid = ASTtypedef(name,streamCT)
		    val hInfo = 
			{name         = name,
			 typ          = streamPtCT,
			 PTlogicalCT  = logicalType,
			 PTphysicalCT = physicalType,
			 isIO         = isIO,
                         isGenerative = isGenerative,
			 transform    = transform,
			 initFun      = initFunName,
                         initFunNoParams =pVersionNameOpt,
                         paramTy = paramCT,
                         formalParams = typedParams,
			 transformArgs = transformArgs,
			 defEvent = fieldName
			 }
		in
		    HS.addId (tid, HI.Stream hInfo);
	            ( structDecls
                    @ [(declTid tid)]
		    @ edecls 
		    @ cnvExternalDecl transWrapperPT
		    @ cnvExternalDecl initFunPT
                    @ List.concat (List.map cnvExternalDecl initFunNoParamsPT))
		end

	in
	    case decl of
		H.SigMain m            => cnvSigMain m
              | H.Type (H.Range r)     => cnvRange r
              | H.Type (H.Event e)     => cnvEventTy e
	      | H.Type (H.Record r)    => cnvRecord r
	      | H.Type (H.Pickle p)    => cnvPickle p
	      | H.Type (H.Map m)       => cnvMap m
	      | H.Type (H.Directory d) => cnvDirectory d 
	      | H.Type (H.Stream s)    => cnvStream s
	end


(* Conversions : declarators *************************************************)

    fun CNVDeclarator(ct,dx) = 
	let fun cnvScope (event,var) = (ct,SOME (Hid "trash"))
	    fun cnvWindow(decr,length,offset) =
		(* This code is similar but not the same as the function
		 cnvWindowSpec below. *)
		let val (ct,var) = mungeTyDecr(ct,PT.ArrayDecr(decr,length))
		    val offset' = cnvStaticInt(offset)
		in
		    case ct of
			Ast.Array(SOME(length',_),ct') =>
			    let val length' = LargeInt.toInt length'
				val _ = 
				    if offset'>=length' orelse offset'<0
				    then X.error ("Window offset must be "^
						"within the array bounds.")
				    else ()
				val info = (ct',length', offset')
				val tid = ASTtypedefGlobal(Hid "window",ct)
			    in
				HS.addExtDecls [declTid tid];
				HS.addId(tid,HI.Window info);
				(Ast.TypeRef tid, var)
			    end
		      | _ => X.fail "In [?:?] both expressions must be constant."
		end
	    fun cnvParamApp(decr, es) = 
		let val (ct,varOpt) = mungeTyDecr(ct, decr)
		    val var = case varOpt of NONE => "" | SOME n => n
                    val tid = ASTtypedef(Hid (var^"_paramApp"), ct)
		    val info = {exprs = es, basety = ct}
                in
		    HS.addExtDecls [declTid tid];
                    HS.addId(tid, HI.Param info);
		    (Ast.TypeRef tid, varOpt)
		end
	in
	    case dx of
		H.HScope x => cnvScope(ct,x)
	      | H.HWindow x => cnvWindow x
	      | H.HParamApp x => cnvParamApp x
	end

(* Specifiers ****************************************************************)
    fun CNVSpecifier {isShadow,rest} spec =
	let fun cnvWindowSpec(PTct,length,offset) =
	    let val (ct,_) = cnvType(isShadow, P.ctToDT PTct)
		val _ = 
		    case rest of [] => () 
		  | _ => X.error "Too many type specifiers."
		fun getInt x =
		    case evalExpr x of
			(SOME i,_,ae,_) => (IntInf.toLarge i
					    handle OverFlow => (X.error "Window constant too large."; 0),
					   ae)
		      | _ => X.fail "In [?:?] both expressions must be constant"
		val length' = getInt length
		val offset' = getInt offset
		val len' = LargeInt.toInt (#1 length')
		val off' = LargeInt.toInt(#1 offset')
		val _ = 
		    if (off' >= len') orelse off'<0 then
			X.error ("Window offset must be within array bounds.")
		    else ()
		val tid = ASTtypedefGlobal(Hid "windowS",AST.Array (SOME (length'),ct))
		val info = (ct,len',off')
	    in
	       HS.addId(tid,HI.Window info);
	       HS.addExtDecls [declTid tid];
	       Ast.TypeRef tid
	    end
	in
	    case spec of 
		H.HWindowSpec (e1,e2,ct) => cnvWindowSpec (ct,e1,e2)
	      | H.HNew => X.fail "New cannot be used in this context."
	      | H.HExists => X.fail "Exists cannot be used in this context."
	end


    fun CNVDeclaration (H.HDecl s) = 
	let
	    fun getDeclaration astStmt =
		case astStmt 
		    of (AST.STMT(AST.Compound(dl,sl),_,_)) => dl
		    |  (AST.STMT(AST.ErrorStmt,_,_)) => X.bug "error statment\n"
		    | _ => X.bug "Unexpected statements in declaration form\n"

	in
	    getDeclaration(cnvStmt s)
	end


(* Conversions : unchanged ***************************************************)

    fun CNVUnop _ = raise (CnvExt "No proper extensions to unary operations.")


(* Conversions : unchanged ***************************************************)
    in
	{
	 CNVExp          = CNVExp,
	 CNVStat         = CNVStat,
	 CNVBinop        = CNVBinop,
	 CNVUnop         = CNVUnop,
	 CNVExternalDecl = CNVExternalDecl,
	 CNVSpecifier    = CNVSpecifier,
	 CNVDeclarator   = CNVDeclarator,
	 CNVDeclaration  = CNVDeclaration
	 }
    end (* end of makeExtensionFuns *)
end 
