structure HInfo =
struct
    structure PT = ParseTree

    type tid = Tid.uid
    type ctype = Ast.ctype
    type sym = Symbol.symbol

    (* the declaration list is needed to propagate temporary variable 
     declarations to outer scope.  They are Ast declarations because those
     are the types we have handy. *)
    type coercion = PT.expression -> (PT.expression * (Ast.declaration list))

    type RangeInfo =
        {
         name : string,
         low : PT.expression,
         high : PT.expression
         }

    type RecordInfo = 
	{ 
	 recName : string,
	 v1Name : string,
	 v1CT : ctype,
	 v2Name : string,
	 v2CT : ctype,
	 toView1 : coercion,
	 toView2 : coercion
	 }

    type PickleInfo =
	{
	 tid         : tid,
	 name        : string,
	 read        : PT.expression,
	 write       : PT.expression,
	 typ         : PT.ctype, 
         initFun     : string,
         initFunNoParams     : string option,
	 readFun     : string,
         writeFun    : string,
	 paramTy     : PT.ctype,
         formalParams : (PT.ctype * string) list
	 }

    type MapInfo =
	{
	 tid         : tid,
	 PTkeyCT     : PT.ctype,
	 keyCT       : ctype,
	 PTrangeCT   : PT.ctype,
	 rangeCT     : ctype,
         readFun     : string,
	 initFun     : string, 
         initFunNoParams : string option,
	 compress    : PT.expression,   (* must supply default. *)
	 decompress  : PT.expression,
	 paramTy : PT.ctype,
	 formalParams : (PT.ctype * string) list,
	 defEvent    : string
	 }

    type DirInfo =
	{
	 name : string,
         tid  : tid,
         PTtypeCT  : PT.ctype,
         initFun : string,
	 initFunNoParams : string option,
	 readFun : string,
	 writeFun : string,
         copyFun : string,
	 paramTy : PT.ctype,
	 formalParams : (PT.ctype * string) list
	 }

    type StreamInfo =
	{
	 name : string,
	 typ  : PT.ctype,
	 PTlogicalCT : PT.ctype,
	 PTphysicalCT : PT.ctype,
         isIO : bool,
	 isGenerative : bool,
         transform : string,
	 transformArgs : PT.expression list,
         initFun     : string,
         initFunNoParams     : string option,
	 paramTy : PT.ctype,
	 formalParams : (PT.ctype * string) list,
	 defEvent : string
	 }

    datatype EventElt = Base | Hier of EventInfo
    withtype EventInfo =
	{
	 name : string,
         elts : (string * ctype * EventElt) list
	 }


    type WindowInfo = ctype * int * int (* type, length, offset *)
    type ParamInfo = 
        {
	  exprs  : PT.expression list, 
          basety : ctype
        }

    datatype Info = 
        Range   of RangeInfo
      | Record  of RecordInfo 
      | Pickle  of PickleInfo
      | Map     of MapInfo 
      | Dir     of DirInfo
      | Stream  of StreamInfo
      | EventTy of EventInfo
      | Window  of WindowInfo
      | Param   of ParamInfo
end




