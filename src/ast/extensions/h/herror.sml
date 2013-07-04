structure HError : HERROR =
struct
        exception HancockFail

	fun dummy (s : string) : unit = (print "Dummy Error Function.";
					 raise HancockFail)

	val   bugRef = ref dummy
	val  failRef = ref dummy
	val errorRef = ref dummy
        val  warnRef = ref dummy

	fun   bug s = ((!bugRef) s; 
		       raise HancockFail)
	fun  fail s = ((!failRef) s;
		       raise HancockFail)
	fun error s = (!errorRef) s
	fun warn s = (!warnRef) s

	fun setup errorState err warn=
	    let fun   bug' s = Error.bug errorState ("H: " ^ s)
		fun error' s = err ("(Hancock) " ^ s)
		fun  fail' s = error' s
                fun warning' s = warn ("(Hancock) " ^ s)
	    in
		  bugRef :=   bug';
		 failRef :=  fail';
		errorRef := error';
		 warnRef := warning';
		()
	    end
end
