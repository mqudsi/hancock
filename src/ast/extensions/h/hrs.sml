(* Interface to the Hancock Runtime System *)

structure HRS =
struct

    structure PT = ParseTree
    structure P  = ParseTreeUtil
    structure CL = CLib
    structure X  = HError
    structure POR = Portability
    structure CL = CLib

    type exp = PT.expression

    (* Internal function for generating new variable names. *)
    val hIdCounter = ref 0
    fun Hid s = (hIdCounter := !hIdCounter + 1;
		 "__H_"^(Int.toString (!hIdCounter))^"_"^s)



    val programName = "HRSprogramName"
    val programNameId = PT.Id "HRSprogramName"

    (* Statements used to generate error messages. *)
    fun errorS str = 
	PT.Compound [PT.Expr(CL.eprintf("%s: " ,[programNameId])),
		     PT.Expr(CL.eprintf(str,[])),
		     PT.Expr(CL.exit 1)
		     ]

    fun errorS2 (str1, data1, str2) = 
	PT.Compound [PT.Expr(CL.eprintf("%s: " ,[programNameId])),
		     PT.Expr(CL.eprintf(str1,data1)),
		     PT.Expr(CL.eprintf(str2,[])),
		     PT.Expr(CL.exit 1)
		     ]

    (* HRS library routines. *)

    fun init (e) = PT.Expr(PT.Call(PT.Id "HRSinit", [e]))
    fun done () = PT.Expr(PT.Call(PT.Id "HRSdone", []))
    fun cleanup () = PT.Expr(PT.Call(PT.Id "atexit",      (* this is now done by the runtime. *)
                                       [PT.Id "HRSdone"]))

   fun openfile(f:exp, mode:exp) =
       PT.Call(PT.Id "HRSopenfile",[f,mode])
   fun closefile(fp:exp) =
       PT.Call(PT.Id "HRSclosefile",[fp])

   (* Hmode is new, exists, don'tcare, not read/write *)
   fun openFileMode(path:exp, Hmode:exp, readonly:exp) =
       PT.Call(PT.Id "HRSopenFileMode",[path,Hmode,readonly])
    (* strings *)
    fun isPositive e        = PT.Call(PT.Id "HRSisPositive", [e])
    fun getChar str res     = PT.Call(PT.Id "HRSgetChar", [str, res])
    fun strtoll (e1,e2,e3)  = PT.Call(PT.Id "HRSstrtoll",[e1,e2,e3]) 
    fun strtoull (e1,e2,e3) = PT.Call(PT.Id "HRSstrtoull",[e1,e2,e3]) 
(*    fun strtold (e1,e2)     = PT.Call( PT.Id "strtold", [e1,e2]) *)
    fun strtol (e1,e2,e3)   = PT.Call(PT.Id "strtol",[e1,e2,e3])
    fun strtoul (e1,e2,e3)  = PT.Call(PT.Id "strtoul",[e1,e2,e3])
    fun strtod (e1,e2)      = PT.Call( PT.Id "strtod", [e1,e2])

    (* SFIO type *)
    val ioCT = P.makeCT [PT.TypedefName "Sfio_t"]

    (* Flags *)
    val readwrite  = P.zero
    val readonly   = P.intX 1
    val dontcare   = P.zero
    val exists     = P.intX 1
    val new        = P.intX 2
    val random     = P.zero
    val ordered    = P.intX 1
    val funcDef   = P.zero
    val constDefS   = P.intX 1   (* if param, then param supplied *)
    val constDefNS  = P.intX ~1  (* param, and not supplied *)
    val paramDef  = P.intX 2

    (* Pickles *)
    val pickleTypeName = "HRSpickleData_s"
    val pickleData = "data"
    val pickleCT       = P.typedef pickleTypeName
    fun pickleOpen (fileName, mode, readOnly, topLevel, pickleSize, 
		    paramSize, paramSet, paramData, read, write, retTyp) = 
	let val rct = P.ptr (P.func POR.int32CT
			    [P.char (* paramSet *), P.voidPtr (* paramData *), 
			     P.charPtr (* path *),  P.ptr pickleCT (* data *), 
			     P.int (* mode *), P.char (* readonly *)   ])
	    val wct = P.ptr (P.func POR.int32CT
			    [P.char (* paramSet *), P.voidPtr (* paramData *), 
			     P.charPtr (* path *),  P.ptr pickleCT (* data *), 
			     P.char (* close *), P.char (* readonly *) ])
	in
	    PT.Cast(retTyp,
		    PT.Call(PT.Id "HRSpickleReg",
			    [fileName, mode, readOnly, topLevel, pickleSize,
			     paramSize, paramSet, paramData,
			     PT.Cast( rct, read),
			     PT.Cast( wct, write)]))
	end

   fun pickleParam p = PT.Call(PT.Id "HRSpickleParam", [p])
   fun pickleInit p = PT.Call(PT.Id "HRSpickleInit", [p])

   fun pickleCopy(to:exp, from:exp) =
       PT.Call(PT.Id "HRSpickleCopy",[from, to])

   val pickleClose = "HRSpickleClose"

    (* Maps *)
    val mapTypeName   = "HRSmap_t"
    val mapCT         = P.typedef mapTypeName
    val mapDefaultCT  = P.typedef "HRSmapDefault_s"	
    val defaultType   = PT.Id "type" 
    val defaultFunc   = PT.Id "f"
    val defaultConst  = PT.Id "constant"
    val defaultFuncCT = P.typedef "HRSmapFunDefault_t"

    val mapCompressCT = P.typedef "HRSmapCompress_s"	
    val compressType = PT.Id "type"
    val compressFn   = PT.Id "cf"
    val decompressFn   = PT.Id "df"
    val mapGenericCompress = P.intX 0
    val mapUserEntryCompress = P.intX 1
    val mapUserStripeCompress = P.intX 2
    


    val rangeDescCT = P.typedef "range_desc_s"
    val keyDescCT   = P.typedef "HRSkey_desc_s"
(* ksf: changing to new key model. This def. will be obsolete 
 *  val keyCT       = P.typedef "HRSkey_s"  
 *)
    val keyCT       = P.void
    val keyLow      = PT.Id "low"
    val keyHigh     = PT.Id "high"
    val keyLevels   = PT.Id "levels"
    val keyRanges   = PT.Id "ranges"    
    val map         = "theMap"

   fun mapStruct name : PT.externalDecl =  
        PT.ExternalDecl(
          PT.Declaration
            ({qualifiers=[],
              specifiers=[PT.Struct
                           {isStruct=true,
                            members=[({qualifiers=[],
                                       specifiers=[PT.TypedefName mapTypeName]},
                                      [(PT.VarDecr map, PT.EmptyExpr)])],
                            tagOpt=SOME name}],
              storage=[]},
             []))

   fun mapAccessX nameX : PT.expression = 
       P.dotX(nameX, PT.Id map)

   fun mapAccessX nameX : PT.expression = nameX

   fun setDefaultFunc defStruct defFuncName = 
       [P.assignS (P.dotX (defStruct, defaultType), funcDef),
	P.assignS (P.dotX (defStruct, defaultFunc), 
		   PT.Cast(defaultFuncCT,PT.Id defFuncName))]

   fun setDefaultParamFunc defStruct defFuncName = 
       [P.assignS (P.dotX (defStruct, defaultType), paramDef),
	P.assignS (P.dotX (defStruct, defaultFunc), 
		   PT.Cast(defaultFuncCT,PT.Id defFuncName))]
       
   fun setDefaultConst (defStruct, defConstName, typ)= 
      [P.assignS (P.dotX (defStruct, defaultType), typ),
       P.assignS (P.dotX (defStruct, defaultConst), 
                  PT.Cast(P.charPtr,P.addrX(PT.Id defConstName)))]

   fun setCompression (compDescStruct, compress, decompress, typ)= 
      [P.assignS (P.dotX (compDescStruct, compressType), typ),
       P.assignS (P.dotX (compDescStruct, compressFn), 
                  PT.Cast(P.voidPtr,compress)),
       P.assignS (P.dotX (compDescStruct, decompressFn), 
                  PT.Cast(P.voidPtr,decompress))]

    fun mapOpen map size default readonly mode topLevel key
        compress decompress = 
	let val ct = P.ptr (P.func POR.int32CT 
			    [POR.puint8CT,POR.puint8CT,POR.int32CT])
	    val dct = P.ptr(P.func POR.int32CT 
			    [POR.puint8CT,POR.int32CT,POR.puint8CT])
	in
	PT.Call(PT.Id "HRSmapOpen",
		[map,size,default,readonly,mode,topLevel,key,
		 PT.Cast( ct,   compress),
		 PT.Cast(dct, decompress)])
	end

    fun mapOpenLL (filename, entrySize, default, readOnly, mode, topLevel, 
		   key, blockSplit, stripeSplit, compressDesc,
		   paramSet, paramSize, paramData) = 
(*	let val ct = P.ptr (P.func POR.int32CT 
			    [P.char (*paramSet*), P.voidPtr (*paramTable*), 
			     POR.puint8CT,POR.puint8CT,POR.int32CT])
	    val dct = P.ptr(P.func POR.int32CT 
			    [P.char (*paramSet*), P.voidPtr (*paramTable*), 
			     POR.puint8CT,POR.int32CT,POR.puint8CT])
	in *)
	PT.Call(PT.Id "HRSLLmapOpen",
		[filename,entrySize,default,readOnly,mode,topLevel,key,
		 blockSplit, stripeSplit,
                 compressDesc,
		 paramSet, paramSize,paramData])
(*	end *)

    (* Expression has return type char* 
     dest should be a pointer to allocated memory. *)
   fun mapGet (map:exp, pn:exp, dest:exp) =
       PT.Call(PT.Id "HRSmapGet",[mapAccessX map,pn,PT.Cast(P.voidPtr,dest)])

   (* Expression has return type void 
    dest should be a pointer to allocated memory. *)
   fun mapPut (map:exp, pn:exp, dest:exp) =
       PT.Call(PT.Id "HRSmapPut",[mapAccessX map,pn,PT.Cast(P.voidPtr,dest)])


   fun mapCopy(to:exp, from:exp) =
       PT.Call(PT.Id "HRSmapCopy",[mapAccessX from, mapAccessX to])

   fun mapToStream(map:exp,low:exp,high:exp, logicalSize:exp) =
	PT.Call(PT.Id "HRSmapToStream",[mapAccessX map,
					P.param(keyCT,low),
					P.param(keyCT,high),
					logicalSize])

   fun mapTestKey(map:exp, key:exp)=
       PT.Call(PT.Id "HRSmapTestKey",[mapAccessX map, key])

   fun mapQuery(map:exp,key:exp)=
	PT.Call(PT.Id "HRSmapQuery",[mapAccessX map, key])


   fun mapRemove(map:exp,key:exp)=
	PT.Call(PT.Id "HRSmapRemove",[mapAccessX map, key])
 
   fun mapRange(map:exp, lo:exp, hi:exp) = 
       PT.Call(PT.Id "HRSmapRanges",
	       [mapAccessX map, PT.Cast(P.voidPtr,lo), PT.Cast(P.voidPtr,hi)])

   val mapClose = "HRSmapClose"



   (* Directories *)
   val dirDname = "name"
   val dirDsize = "size"
   val dirDoffset = "offset"
   val dirDread = "readFn"
   val dirDwrite = "writeFn"
   val dirDparamSet = "paramSet"
   val dirDparamData = "paramData"
   val dirDFieldName = "HRSdirField_t"
   val dirDFieldNameSt = "HRSdirField_s"
   val dirName = "HRSdir_t"
(* XXX old *)
   val dirReadFnType = P.ptr (P.func P.void
			    [P.char, P.voidPtr, P.charPtr, P.charPtr, P.int, P.int, P.int])
(* XXX end old *)
   val dirReadFnType = P.ptr (P.func P.void
			    [P.char (* set *), P.voidPtr (* paramTable *), P.voidPtr (* extParamTable *), 
                             P.voidPtr (* dirStruct *), P.charPtrPtr (* fileNames *), 
			     P.int, (* mode *) P.char (* readonly *)])
   val dirWriteFnType = P.ptr (P.func P.void
			    [P.char (* set *), P.voidPtr (* extParamTable *), P.voidPtr (* dirStruct *), 
			     P.charPtrPtr (* fileNames *), P.char(* close *), P.char (* readonly *)])
   fun dirReg(name:exp, fieldNames:exp, length:exp, size:exp, mode:exp, readonly:exp, toplevel:exp,
	      paramSet:exp, paramData:exp, extParamTableSize:exp, readFun:exp, writeFun:exp) = 
       PT.Call(PT.Id "HRSdirReg",[name,fieldNames,length,size,mode,readonly, toplevel,
				  paramSet, paramData, extParamTableSize,
				  PT.Cast(dirReadFnType, readFun),
				  PT.Cast(dirWriteFnType, writeFun)])
   val dirClose = "HRSdirClose"

   (* Streams *)
   val streamTypeName = "HRSstream_t"
   val streamCT = P.typedef streamTypeName

   val isIO = P.intX 1
   val isBin = P.intX 2
   val isGenerative = P.intX 3

   val streamKeep = P.intX 1

   (* Streams--- slices *)

   val sliceTypeName = "HRSslice_s"
   val sliceCT = P.typedef sliceTypeName
   val sliceStart = "begin"
   val sliceStop = "end"
   val slices = "description"

   val sortSubKeyCT = P.typedef "HRSsortSubKey_s"
   val streamTransCT = 
       let val params =
	   [(P.makeDT [PT.Char],
	     PT.PointerDecr (PT.VarDecr "physical")),
	    (P.makeDT [PT.Char], 
             PT.PointerDecr (PT.VarDecr "logical"))]
       in
	   P.makeCT [ PT.Pointer (P.makeCT [PT.Function{retType = P.int,
							params  = params}])
		     ]
       end

   fun streamCastX transform = PT.Cast(streamTransCT,transform)

   val streamIOTransCT = 
       let val params =
	   [(P.ctToDT ioCT,
	     PT.PointerDecr (PT.VarDecr "file")),
	    (P.makeDT [PT.Char], 
             PT.PointerDecr (PT.VarDecr "logical"))]
       in
	   P.makeCT [ PT.Pointer (P.makeCT [PT.Function{retType = P.int,
							params  = params}])
		     ]
       end

   fun streamIOCastX transform = PT.Cast(streamIOTransCT,transform)
       
   fun streamReg (streamDir,paramSize, paramSet, paramValue, strmTyp, 
		  physicalSize,logicalSize, wrapperX) = 
         PT.Call(PT.Id "HRSstreamReg", 
                 [streamDir, paramSet, paramValue, paramSize, strmTyp, 
		  physicalSize, logicalSize, PT.Cast(P.voidPtr, wrapperX)])


   fun streamStart (st:exp,filter:exp,numSlicesX:exp, slicesX:exp) =
       PT.Call(PT.Id "HRSstreamStart",[st, 
				       PT.Cast(P.voidPtr, filter),
				       numSlicesX, slicesX])

   fun streamNext (st:exp, record:exp) =
       PT.Call(PT.Id "HRSstreamNext",[st,PT.Cast(P.charPtr,record)])
       
   fun streamFinish stream = PT.Call(PT.Id "HRSstreamFinish", [stream])

   val filterCT = 
       let val params = [(P.makeDT [PT.Char],PT.PointerDecr (PT.VarDecr "r"))]
       in       
	   P.makeCT [ PT.Pointer (P.makeCT [PT.Function{retType = P.int,
							params  = params}])
		     ]
       end
   
   val offset = "offset"
   val size = "size"

   val sortDiscCT =
       (P.makeCT [PT.Pointer (P.makeCT [PT.TypedefName "Rsdisc_t"])])

end
