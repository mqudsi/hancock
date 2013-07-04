(* Mangle and Demangle event names. *)

structure Mangle : 
sig
    val   mangle : (string list * string) -> string
    val demangle : string -> string
end =
struct
    
open Substring

fun mangle (eventNameList,varName) = 
    let 
	fun encode_sz s = 
	    let val sz = String.size s
		val char_sz = Char.chr ((Char.ord #"a") + sz)
		val char_sz = if Char.isAlphaNum char_sz then char_sz
				else #"_"
	    in
		Char.toString char_sz
	    end
        fun munge ([],s) = (encode_sz s)^s
          | munge ((hd::tl),s) = (encode_sz hd)^hd^"_"^munge(tl,s)
        val newName = case eventNameList of
	                 [""] => (encode_sz "record_")^"record_"
			|  _  => munge(eventNameList,varName)
    in
	("__H_E_" ^newName) (* hid for event variables *)
    end

fun decode_sz #"_" = 0
  | decode_sz char_sz = (Char.ord char_sz) - (Char.ord #"a")
	
fun rewrite_id (id:substring) : string = 
    (let val body = slice(id,6,NONE)
	 fun process_field ss =
	     case first ss of
		 NONE => []
	       | SOME ch => 
		     let val sz = decode_sz ch 
			 val rest = (triml 1 ss)
			 val (name,rest) = 
			     if sz = 0 then splitl (fn c => c = #"_") rest
			     else splitAt(rest,sz)
		     in
			 name :: (process_field (triml 1 rest))
		     end
	 fun format_fields [hd] = (string hd)
	   | format_fields [hd,tl] = (string hd)^"::"^(string tl)
	   | format_fields (hd::tl)=(string hd)^"."^(format_fields tl)
	   | format_fields [] = ""
     in 
	 format_fields (process_field body)
     end
	 handle Subscript => ("<Mangling Error" ^(string id)^">"))

(* Still somewhat broken since __H_E_ should be preceded by whitespace! *)    
fun demangle s =
    let fun doit s = 
	let val (pref,suff) = position "__H_E_" s
	in
	    if isEmpty suff then (string s)
	    else
		let val (id,rest) = splitl Char.isGraph suff
		in
		    ((string pref)^
		     (rewrite_id id)^
		     (doit rest))
		end
	end
    in
	doit (extract (s,0,NONE))
    end

end