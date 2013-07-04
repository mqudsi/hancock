(* Interface to the C library. *)
structure CLib =
struct
    structure PT = ParseTree
    structure P = ParseTreeUtil
    structure X = HError

    val ERANGE = P.intX 34
    val errno = PT.Id "errno"
	
    fun bzero el = PT.Call(PT.Id "bzero",el)
    fun strlen e = PT.Call(PT.Id "strlen",[e])
    fun malloc e = PT.Call(PT.Id "malloc",[e])
    fun free e = PT.Call(PT.Id "free", [e])
    fun strcpy (e1,e2) = PT.Call(PT.Id "strcpy", [e1,e2])
    fun strcmp (e1,e2) = PT.Call(PT.Id "strcmp", [e1,e2])
    fun strcat(e1,e2) = PT.Call(PT.Id "strcat", [e1,e2])

    val optopt = PT.Id "optopt"
    val opterr = PT.Id "opterr"
    fun getopt (e1,e2,e3) = PT.Call(PT.Id "getopt",[e1,e2,e3])

    fun memcpy (e1,e2,size) = 
	PT.Call(PT.Id "memcpy", [PT.Cast(P.voidPtr, e1),
				 PT.Cast(P.voidPtr, e2),
				 size])



    fun sfprintf (e1,e2,e3) = PT.Call(PT.Id "sfprintf", [e1,e2]@e3)
    fun eprintf (s:string,es) = (* used to print error messages. *)
	sfprintf(P.addrX (PT.Id "_Sfstderr"),PT.String s,es)

    fun readStr (fname,s) = PT.Expr(PT.Call(PT.Id "sfscanf",
		 			 [PT.Id fname, PT.String s]))
    fun readArg (fname,s,iX) = PT.Expr(PT.Call(PT.Id "sfscanf",
			 			 [PT.Id fname, PT.String s, iX]))
    fun readLine fname = readStr(fname,"\n")

    fun writeStr (fname, s) = PT.Expr(PT.Call(PT.Id "sfprintf",
						[PT.Id fname, PT.String s]))
    fun writeArg (fname, s, iX) = PT.Expr(PT.Call(PT.Id "sfprintf",
						    [PT.Id fname, PT.String s, iX]))
    fun writeLine fname = writeStr (fname, "\n")

    fun exit i = PT.Call(PT.Id "exit", [P.intX i])

    val argv = PT.Id "argv"
    val argc = PT.Id "argc"
    val optarg = PT.Id "optarg"
    val optind = PT.Id "optind"

    (* Opens a "file" that exists only in memory for reading and writing. 
       The returned expression is a file pointer.
       The argument is an l-value to store the file pointer in.
       (No error checking is done here.)
     *)

    fun sfclose e = PT.Call(PT.Id "sfclose",[e])
    fun sfseek (fptr,offset,typ) = PT.Call(PT.Id "sfseek",[fptr,offset,typ])
    fun fopenVirtual f =
	P.assignX(f, PT.Call(PT.Id "sfnew",
			     [P.zero, (* Sfio_t*)
			      P.zero, (* Buffer *)
			      P.intX (~1), (* Buffer size *)
			      P.intX (~1), (* File descriptor *)
			      P.intX 7]))  (* Flags for a read|write|string *)
end
