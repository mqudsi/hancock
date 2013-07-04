(* parse-to-ast.sml *)

structure ParseToAst : PARSE_TO_AST =
struct

  type astBundle =
    {ast: Ast.ast,
     tidtab: Bindings.tidBinding Tidtab.uidtab,
     errorCount: int,
     warningCount: int,
     auxiliaryInfo: {aidtab: Tables.aidtab,
	             implicits: Tables.aidtab,
                     env: State.symtab}}

  fun progToState ({tidtab, auxiliaryInfo={aidtab, implicits, env}, ...} : astBundle) =
      State.STATE({ttab=tidtab,atab=aidtab,implicits=implicits},env)

  fun fileToAst' errStrm (sizes: Sizes.sizes, stateInfo: State.stateInfo) inFile
         : astBundle = 
      let
	(* suppress underscores to make error message more readable *)
	val suppressPidUnderscores = !PPLib.suppressPidUnderscores
	val suppressTidUnderscores = !PPLib.suppressTidUnderscores
	val _ = (PPLib.suppressPidUnderscores := true;
		 PPLib.suppressTidUnderscores := true)
	val errState = Error.mkErrState errStrm
	val p = Parser.parseFile errState inFile
	val result = BuildAst.makeAst (sizes,stateInfo,errState) p
      in
	PPLib.suppressPidUnderscores := suppressPidUnderscores;
	PPLib.suppressTidUnderscores := suppressTidUnderscores;
	result
      end
    
  (* HANCOCK modifications to detect multiple occurrences 
     of main: one from programmer, one from sigmain translation. *)
  fun fileToAst inFile = 
      let val r =  fileToAst' TextIO.stdErr (Sizes.defaultSizes, State.INITIAL) inFile
          val foundError = AstChkDups.chkdupfns(#ast r, TextIO.stdErr, "main")
      in r end

  fun fileToC x = 
      let val {ast, tidtab, ...} = fileToAst x
       in PPLib.ppToStrm (PPAst.ppAst NONE () tidtab) TextIO.stdOut ast
      end

end (* structure ParseToAst *)
