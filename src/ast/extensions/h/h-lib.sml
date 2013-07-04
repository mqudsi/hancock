(* Some common utilities. *)

structure HLib =
struct

    structure X = HError

    exception Abort

    (* Extract the first element where f returns true and return the
    remainder of the list. *)
    fun extract f [] = NONE
      | extract f (l as hd::tl) = 
	let val tmp = ref NONE
	    fun loop [] = []
	      | loop (hd::tl) = if f hd then (tmp := SOME hd; tl) 
				else hd::(loop tl)
	    val res = loop l
	in
	    case !tmp of
		NONE => NONE
	      | SOME elt => SOME (elt,res)
	end

    (* Remove SOME from an option type.  It is an error if the value is NONE *)
    fun deSome v = 
	(case v of 
	     NONE => X.bug "Found NONE where SOME expected."
	   | SOME v' => v')

    fun somePair (x,y) = (SOME x,SOME y)
    fun ifSome f v = (case v of 
			  NONE => NONE 
			| SOME v' => SOME (f v'))

    (* Take an option list and create a list of the contents of the options. *)
    fun coalesceOption l =
	let fun loop [] = []
	      | loop ((SOME hd) :: tl) = hd :: (loop tl)
	      | loop (NONE :: tl) = raise Abort
	in
	    SOME (loop l) handle Abort => NONE
	end
    
    fun appendPair (x,y) (lx,ly) = (x@lx,y@ly)
    fun appendTriple (x,y,z) (lx,ly,lz) = (x@lx, y@ly, z@lz)
    fun appendQuad (w,x,y,z) (lw,lx,ly,lz) = (w@lw, x@lx, y@ly, z@lz)
    fun appendQuint (u,w,x,y,z) (lu,lw,lx,ly,lz) = (u@lu, w@lw, x@lx, y@ly, z@lz)
    fun appendSext (t,u,w,x,y,z) (lt,lu,lw,lx,ly,lz) = (t@lt, u@lu, w@lw, x@lx, y@ly, z@lz)
    fun appendSept (s,t,u,w,x,y,z) (ls,lt,lu,lw,lx,ly,lz) = (s@ls,t@lt, u@lu, w@lw, x@lx, y@ly, z@lz)
    fun appendOct (r,s,t,u,w,x,y,z) (lr,ls,lt,lu,lw,lx,ly,lz) = (r@lr,s@ls,t@lt, u@lu, w@lw, x@lx, y@ly, z@lz)

    type 'a emitter = ('a -> unit) * (unit -> 'a list) * ('a list -> unit)
    fun genEmitter () : 'a emitter = (* STATE *)
	let val a = ref [] 
	    fun add b = (a := b :: (!a))
	    fun get () = (!a)
	    fun reset b = (a := b)
	in
	    (add,get,reset)
	end

    (* Maps a function returns a string across a list and returns a
     compound string *)
    fun smap f [] = ""
      | smap f (hd::tl) = (f hd) ^ (smap f tl)


end
