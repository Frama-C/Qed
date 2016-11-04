open Logic

module Make (T : Logic.Term) =
struct

  open T
	
  type compute = term -> term array -> term
    
  let rec unify mgu = function
      
    | [] -> Array.map (function Some e -> e | None -> raise Not_found) mgu

    | (Pany,_) :: eqs -> unify mgu eqs

    | (Pvar k,e) :: eqs ->
	( match mgu.(k) with
	    | Some e' ->
		if e' != e then raise Not_found
	    | None ->
                mgu.(k) <- Some e ) ;
	unify mgu eqs

    | (Pint z,e) :: eqs ->
	( match repr e with
	    | Kint z' when Z.equal z z' -> unify mgu eqs
	    | _ -> raise Not_found )

    | (Ptrue,e) :: eqs ->
	( match repr e with
	    | True -> unify mgu eqs
	    | _ -> raise Not_found )

    | (Pfalse,e) :: eqs ->
	( match repr e with
	    | False -> unify mgu eqs
	    | _ -> raise Not_found )

    | (Pfun(f,xs),e) :: eqs ->
	( match repr e with
	    | Fun(g,ys) when Fun.equal f g ->
		unify mgu ( List.combine xs ys @ eqs )
	    | _ -> raise Not_found )

  let rec rewrite_any compute f xs = function
    | [] -> None
    | r :: others ->
	try

	  if not r.active then raise Not_found ;

	  match r.pattern with
	    | Pany | Pvar _ | Pint _ | Ptrue | Pfalse -> raise Not_found
	    | Pfun(_,ps) ->
		let env = unify 
		  (Array.make r.pvars None)
		  (List.combine ps xs) in
		begin
		  match T.repr (compute r.guard env) with
		    | True ->
			let e = compute r.value env in
			r.activated <- true ;
			Some e
		    | _ -> raise Not_found
		end

	with _ ->
	  rewrite_any compute f xs others

  let rewrite rules compute f xs =
    rewrite_any compute f xs rules

  let rewrite_rule_at_pos r compute term position =
    try begin
      let subterm = T.subterm term position in
      try
        let env = unify (Array.make r.pvars None) [r.pattern, subterm] in
        let guard = compute r.guard env in
        let simpl = compute r.value env in
        let term = T.change_subterm term position simpl in
        (guard,term)
      with Not_found ->
        Format.printf "Warning: rule %s cannot be applied.@." r.name;
        (T.e_true,term)
    end
    with
      | Failure _ ->
          Format.printf
            "Warning: cannot extract subterm at desired position.@.";
          (T.e_true, term)
end

(* ------------------------------------------------------------------------ *)
(* --- Rewriter for Ground                                              --- *)
(* ------------------------------------------------------------------------ *)

open Ground
module R = Make(T)

let rec do_simplify omega sigma m e =
  match T.repr e with
    | Bvar(k,_) when k < Array.length sigma -> sigma.(k)
    | _ ->
	if T.is_atomic e then e else
	  try Intmap.find (T.id e) !m
	  with Not_found ->
	    let r =
	      match T.repr e with
		| Fun(f,es) ->
		    let rs = List.map (do_simplify omega sigma m) es in
		    begin
		      match R.rewrite (omega f) compute f rs with
			| Some r ->
			    (* cutting circularities *)
			    m := Intmap.add (T.id e) r !m ;
			    (* recursively rewrite *)
			    do_simplify omega sigma m r
			| None -> T.e_fun f rs
		    end
		| _ -> T.e_map ~closure:false (do_simplify omega sigma m) e
	    in (* final result *)
	    m := Intmap.add (T.id e) r !m ; r

and compute e args = do_simplify Globals.get_rules args (ref Intmap.empty) e

let simplify e = do_simplify Globals.get_rules [| |] (ref Intmap.empty) e

let subst e args = do_simplify (fun _ -> []) args (ref Intmap.empty) e

let rewrite_rule_at_pos r t pos = R.rewrite_rule_at_pos r subst t pos
