signature HSTATE =
sig
    exception Absent
    exception EmptyDecls (* used when popping an empty stack. *)

    type tid = Tid.uid

    val lookId : tid -> HInfo.Info option
    val  getId : tid -> HInfo.Info  (* may raise Absent *)
    val  addId : (Tid.uid * HInfo.Info) -> unit

    (* These functions hold hidden state used to float declarations up
    to the nearest enclosing statement.  The code uses addDecl or
    addDecls to add the declarations to be floated.  These are
    retrieved and spliced into the code when processing a compound
    statement using popDecls. *)

    (* Almost all of these may raise EmptyDecls except pushDecls. *)
    val popDecls : unit -> Ast.declaration list
    val pushDecls : unit -> unit
    val addDecl : Ast.declaration -> unit
    val addDecls : Ast.declaration list -> unit
    val peekDecls : unit -> Ast.declaration list

    val addExtDecls : Ast.externalDecl list -> unit
    val popExtDecls : unit -> Ast.externalDecl list

    val PTaddExtDecls : ParseTree.externalDecl list -> unit
    val PTpopExtDecls : unit -> ParseTree.externalDecl list 
end


