structure HState : HSTATE =
struct

  exception Absent
  exception EmptyDecls

  (* symbol table binary maps *)
  structure IDT = UidtabImpFn (structure Uid=Tid)

  type tid = Tid.uid

  val hstate = (IDT.uidtab () : HInfo.Info IDT.uidtab)

  fun lookId s = (IDT.find (hstate,s))
  fun getId s = (case IDT.find (hstate,s) of 
		      NONE => raise Absent
		    | SOME e => e)

  fun addId (s,ht) = (IDT.insert(hstate,s,ht))
  
  (* Stack of declarations *)
  val decls = ref ([] : Ast.declaration list list)

  fun popDecls () =
      (case !decls of 
	   []       => raise EmptyDecls
	 | (hd::tl) => (decls:=tl; hd))

  fun pushDecls () = (decls := [] :: (!decls))

  fun addDecl s =
      (case !decls of
	   []       => raise EmptyDecls
	 | (hd::tl) => (decls := (s::hd)::tl))

  fun addDecls ss =
      (case !decls of
	   []       => raise EmptyDecls
	 | (hd::tl) => (decls := (ss @ hd) :: tl))

  fun peekDecls () =
      (case !decls of
	   []       => raise EmptyDecls
	 | (hd::tl) => hd)  


  val extDecls = ref [] : (Ast.externalDecl list) ref

  fun addExtDecls x = (extDecls := x @ !extDecls) 

  fun popExtDecls () = 
      let val x = !extDecls
      in
	  extDecls := [];
	  x
      end

  val PTextDecls = ref [] : (ParseTree.externalDecl list) ref

  fun PTaddExtDecls x = (PTextDecls := x @ !PTextDecls)
  fun PTpopExtDecls () = 
      let val x = !PTextDecls
      in
	  PTextDecls := [];
	  x
      end

end
