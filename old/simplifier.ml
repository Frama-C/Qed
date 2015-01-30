(* -------------------------------------------------------------------------- *)
(** Term Simplifier                                                           *)
(* -------------------------------------------------------------------------- *)

open Logic

exception NoSolution

class type ['a] eqs =
object
  method repr : 'a -> 'a
  method equal : 'a -> 'a -> maybe
  method merge : 'a -> 'a -> unit
  method iter : ('a -> 'a -> unit) -> 'a -> unit
end

class type ['a] simplifier =
object
  method copy : 'a simplifier 
  method compute : 'a eqs -> 'a -> 'a
  method assume : 'a eqs -> 'a -> unit
  method normalize : 'a eqs -> unit
end

type 'a t = 'a simplifier

module Make(T : Term) =
struct

  open T
  module E = Equality.Make(T)

  class state 
    (sigma:Vars.t) (* set of bound variables *)
    (eqs0:E.eqs) (* set of equalities *)
    (hyps0:Tset.t) (* set of properties assumed *)
    (plugins:term simplifier list) (* simplifiers with properties from eqs0 and hyps *)
    = 
  object(self)

    val mutable eqs : E.eqs = eqs0
    val mutable tomerge : (term * term) list = []
    val mutable hypothesis : Tset.t = hyps0

    method repr x = E.repr_of eqs x
    method equal a b = E.are_equal eqs a b

    method merge a b = 
      let a0 = E.repr_of eqs a in
      let b0 = E.repr_of eqs b in
      if a0 != b0 then tomerge <- (a0,b0) :: tomerge
   
    method iter f x = 
      let x0 = E.repr_of eqs x in
      Tset.iter (fun y -> f y x0) (E.class_of eqs x)

    method normalize =
      let facts = E.class_of eqs e_true in
      let new_facts = Tset.diff facts hypothesis in
      hypothesis <- Tset.union facts hypothesis ;
      let eqs = (self :> term eqs) in
      List.iter
	(fun s -> Tset.iter (s#assume eqs) new_facts ; s#normalize eqs) 
	plugins

    method fixpoint =
      while tomerge <> [] do
	eqs <- List.fold_left (fun eqs (a,b) -> E.merge eqs a b) eqs tomerge ;
	tomerge <- [] ;
	self#normalize ;
      done

    method compute e =
      match T.repr e with
	| Bind _ -> e
	| _ ->
	    let e = T.e_map self#compute e in
	    List.iter
	      (fun s ->
		 Tset.iter
		   (fun x -> self#merge e (s#compute (self :> term eqs) x))
		   (E.class_of eqs e)
	      ) plugins ;
	    self#fixpoint ;
	    let e' = E.repr_of eqs e in
	    if e==e' then e else self#compute e'

    method copy ?bind () =
      let sigma = match bind with None -> sigma | Some x -> Vars.add x sigma in
      let plugins = List.map (fun s -> s#copy) plugins in
      new state sigma eqs hypothesis plugins

    method assume h = eqs <- E.assume eqs h
	
    method push hs p =
      let state = self#copy () in 
      (* if current state is contradictory, [p] is not true ! *)
      try
	Array.iter state#assume hs ;
	state#normalize ;
	state#fixpoint ;
	state#simplify p
      with E.Contradiction | NoSolution -> 
	T.e_true
	      
    method simplify e =
      match T.repr e with

	| True | False | Kint _ | Kreal _ -> e

	| Times _ | Add _ | Mul _ | Div _ | Mod _
	| Leq _ | Lt _ | Var _ | Aset _ | Aget _ | Rdef _ | Rget _
	    -> self#compute e

	| Eq _ | Neq _ | And _ | Or _ | Not _ | Fun _ | Apply _ 
	    -> T.e_map (self#simplify) e

	| Imply( hs , p ) ->

	    let hs = Array.of_list hs in
	    Array.iteri
	      (fun i h ->
		 hs.(i) <- T.e_true ;
		 hs.(i) <- self#push hs h
	      ) hs ;
	    let q = self#push hs p in
	    T.e_imply (Array.to_list hs) q

	| If(e,a,b) ->
	    
	    let e = self#simplify e in
	    begin
	      match T.repr e with
		| True -> self#simplify a 
		| False -> self#simplify b
		| _ ->
		    let a = self#push [| e |] a in
		    let b = self#push [| T.e_not e |] b in
		    T.e_if e a b
	    end

	| Bind ( binder , x , f ) ->

	    if not (Vars.mem x sigma)
	    then 
	      let f = (self#copy ~bind:x ())#simplify f in
	      T.e_bind binder x f
	    else 
	      let y = T.fresh ~vars:sigma (tau_of_var x) ~basename:(Var.basename x) in
	      let f = T.e_subst x (e_var y) f in
	      let f = (self#copy ~bind:y ())#simplify f in
	      T.e_bind binder y f

  end

  let simplify plugins t =
    try
      let plugins = List.map (fun s -> s#copy) plugins in
      let state = new state Vars.empty E.identity Tset.empty plugins in
      state#simplify t
    with E.Contradiction -> raise NoSolution

end
