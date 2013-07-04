(* Comments:
 
 The parser wraps all compound statements with a statement
 extension. In conjunction with a bit of state in the hancock
 environment, this allows us to float temporary variable declarations
 up to the appropriate level.

 The parser also wraps assignment expressions with a bogus extension
 expression, so that we can correctly process map declarations.

 Declarations are very confusing. I have chosen arbitrarily to put the scoping
 operator in notypeDirectDeclarator.

 Some syntax errors cannot be caught during parsing.  So I have augmented,
 HRecord, ....
with error fields.  If the field is NONE then the rest is valid.  IF the field
is SOME the associated string is the relevant error message.  Most of these 
errors are probably uncommon.  The locations are not currently accessible 
during parsing so I am assuming that we can get approximate locations during 
elaboration when the errors will be reported.
*)

structure ParseTreeExt = 
    struct
	
        datatype Hmode = Exists | New | Either
	    
	datatype HOperator = 
	    HView | HIndex 
	  | HUnion | HRemove | HDiff | HDot | HArrow | HMember
	  | HMapRemove | HMapMember | HMapTestKey

	type HPath = string list

	type HScope = HPath * string
	    
        type ('exp) HRange = 
            {
             name       : string,
             low        : 'exp,
             high       : 'exp
             }
	    
	(* 'stmt is either compound in which case a function body was
	 supplied, or is a PT.Expr (exp) in which case an expression
	 was supplied. *)
	type ('ct, 'decr, 'stmt) HRecord = 
	    { 
	     errorMsg   : string option,
 	     name       : string option,
	     view1      : string,
	     view2      : string,
	     toView1    : (string * 'stmt) option,
	     toView2    : (string * 'stmt) option,	     
	     members    : ('ct * ('ct option) * ('decr list)) list
	     }

	type ('ct, 'decr, 'exp) HPickle =
	    {
	     name      : string,
             params    : ('ct * 'decr) list,
	     read      : string,
             readArgs  : 'exp list,
             write     : string,
             writeArgs : 'exp list,
             typ       : 'ct
	     }

        datatype ('ct, 'exp) HKey = 
	    SKey of 'ct
          | LKey of ('exp * 'exp)

        datatype 'exp HMapPApp =
            NonParam of 'exp
          | Param of (string * 'exp list)

	type ('ct,'decr,'exp) HMap = 
            { 
	     errorMsg  : string option,
	     name      : string,
	     params    : ('ct * 'decr) list,
	     keyType   : ('ct, 'exp) HKey,
	     rangeType : 'ct,
             split     : ('exp * 'exp) option,  (* split should be present if key type is LKey *)
	     default   : 'exp HMapPApp option,
	     compress  : 'exp HMapPApp option,
	     decompress: 'exp HMapPApp option
	     }
	    
	type ('ct,'decr, 'exp) HDirMember =
	    { 
	     declaration: ('ct * ('decr * 'exp) list) 
	     (* expression = default -- emptexpr is NONE *)
	     }

	type ('ct,'decr,'exp) HDirectory =
	    { 
	     name : string,
             params    : ('ct * 'decr) list,
	     members : ('ct,'decr,'exp) HDirMember list
	     }

	type ('ct, 'decr, 'exp) HStream = 
	    { 
	     name          : string,
             params        : ('ct * 'decr) list,
	     logicalType   : 'ct,
	     physicalType  : 'ct,
	     transform     : string,
	     transformArgs : 'exp list
	     }

	type ('ct, 'decr) HEventTy =
	    {
	     name   : string,
             elts   : ('ct * 'decr) list
	    }

	    
	datatype ('ct, 'decr, 'exp, 'stmt ) HType = 
            Range     of ('exp) HRange
	  | Record    of ('ct, 'decr, 'stmt ) HRecord 
	  | Pickle    of ('ct, 'decr, 'exp) HPickle
	  | Map       of ('ct, 'decr, 'exp) HMap 
	  | Directory of ('ct, 'decr, 'exp ) HDirectory
	  | Stream    of ('ct, 'decr, 'exp ) HStream	    
          | Event     of ('ct, 'decr) HEventTy
	    	    
	type ('ct, 'decr, 'stmt) HEvent = 
	    { 
	     name       : HPath,
	     arg        : ('ct * 'decr) option,
	     body       : 'stmt
	     }

        datatype ('exp) HFilter =
            FilterFun   of string
          | FilterExpr  of string * 'exp

        datatype ('exp) HDetect =
            DetectFun   of string
          | DetectExpr  of string * 'exp * 'exp * 'exp
            

	type ('ct, 'decr, 'exp, 'stmt) HIterates = 
	    { 
	     errorMsg : string option,
	     over     : 'exp,
	     range    : 'exp option * 'exp option, (* low-high range *)
	     filter   : 'exp HFilter option,
	     sort     : (string list) option, (* SOME [] = sort on whole record *)
	     detector : 'exp HDetect option,
	     events   : (('ct, 'decr, 'stmt) HEvent) list
	     }
	    
	type ('ct,'decr, 'exp) HSigParam = 
	    { 
	     errorMsg  : string option,
	     name      : string,
	     typ       : 'ct * 'decr,
	     default   : 'exp option,
	     switch    : string, (* switch = "" means first parameter to
				    sig_main.  argv[0] *)
	     hasArg    : bool, (* true = has an argument. *)
	     msg       : string option (* user supplied string argument. *)
	     }
	    
	type ('ct, 'decr, 'exp, 'stmt) HSigMain = 
	    { 
	     retType : 'ct,
	     params  : (('ct,'decr,'exp) HSigParam) list,
	     body    : 'stmt
	     } 
	    
	datatype ('ct, 'decr, 'exp, 'stmt) HExternal = 
	    SigMain of ('ct, 'decr, 'exp, 'stmt) HSigMain 
	  | Type    of ('ct, 'decr, 'exp, 'stmt ) HType
	    
	datatype ('ct, 'decr, 'exp, 'stmt) HStatement = 
	    HCompound of 'stmt
	  | HMapCopy of 'exp * 'exp
	  | HIterate of ('ct, 'decr, 'exp, 'stmt) HIterates

        datatype ('stmt) HDeclaration = 
            HDecl of 'stmt

        type ('exp) HEventConstr = (string * 'exp option) list
	    
	datatype ('ct,'exp) HExpression = 
	      HAssign of 'exp
	    | HEventC of 'exp HEventConstr
	    | HEventEmpty of 'ct 

	datatype ('decr, 'exp) HDecr =
	      HScope  of string list * string
	    | HWindow of 'decr * 'exp * 'exp
            | HParamApp of 'decr * 'exp list

	datatype ('ct, 'exp) HSpec =
	    HWindowSpec of 'exp * 'exp * 'ct (* size,offset,type *)
	  | HNew 
	  | HExists

	type operatorExt = HOperator

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    expressionExt = ('ct,'exp) HExpression

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    specifierExt = ('ct,'exp) HSpec

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declaratorExt = ('decr,'exp) HDecr
	    
	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    statementExt = ('ct,'decr,'exp,'stmt) HStatement

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    declarationExt = ('stmt) HDeclaration

	type ('spec,'decr,'ct,'dt,'oper,'exp,'stmt) 
	    externalDeclExt = ('ct, 'decr, 'exp, 'stmt) HExternal 
    end




