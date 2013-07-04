signature CONFIG =
sig
  val DFLAG : bool
  val HFLAG : bool  (* HANCOCK *)

  structure ParseControl : PARSECONTROL
  structure TypeCheckControl : TYPECHECKCONTROL
end