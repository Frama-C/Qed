open Logic

module Make(T : Term) =
struct

  open T

  type eqs = T.term Simplifier.eqs

  let rec get (eqs:eqs) m k v = 
    match T.repr m with
      | Aset( m0 , k0 , v0 ) ->
	  begin
	    match eqs#equal k k0 with
	      | Yes -> eqs#merge v v0 ; v0
	      | No -> get eqs m0 k v
	      | Maybe -> v
	  end
      | _ -> v

  class arrays =
  object(self)

    method copy = (self :> term Simplifier.t)
    method normalize (_:eqs) = ()
    method assume (_:eqs) (_:term) = ()
    method compute (eqs:eqs) e =
      match T.repr e with
	| Aget(m,k) -> get eqs m k e
	| _ -> e

  end

  let simplifier () = new arrays

end

