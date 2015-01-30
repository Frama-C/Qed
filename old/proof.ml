(* -------------------------------------------------------------------------- *)
(*  Proof Representation and Checking                                         *)
(* -------------------------------------------------------------------------- *)

open Logic

module Make(T : Term) =
struct

  open T

  module E = Equality.Make(T)

  (* Invariants: 
     Si = And { Hi | i <> j }
     Pi = Si(Hi)
     S = And { Hi,Pi | all i }
  *)

  type context = {
    full : E.eqs ;                       (* S *)
    hyps : (term * term * E.eqs) list ;  (* ( Hi,Pi,Si ) *)
    domain : Vars.t ;                    (* Vars(Hi)     *)
  }

  let add_hypothesis env h =
    let full = ref env.full in
    let hyps = 
      List.map
	(fun (hi,_,si) ->
	   let si' = E.assume si h in
	   let pi' = E.compute si' hi in
	   full := E.assume !full pi' ;
	   hi , pi' , si')
	env.hyps
    in
    let p = E.compute env.full h in
    full := E.assume !full h ;
    full := E.assume !full p ;
    {
      full = !full ;
      hyps = ( h , p , env.full ) :: hyps ;
      domain = Vars.union env.domain (T.vars h) ;
    }

  let empty = { full = E.identity ; hyps = [] ; domain = Vars.empty }

  let assume env h = 
    try Some(add_hypothesis env h) 
    with E.Contradiction -> None

  let assume_all env hs =
    try Some(List.fold_left add_hypothesis env hs)
    with E.Contradiction -> None

  let check_true env p = T.is_true (E.compute env.full p)
  let check_false env p = T.is_false (E.compute env.full p)

  type proof =
    | Compute
    | Assumptions of proof
    | Cut of ( term * proof ) list * proof
    | Cases of ( term * proof ) list * proof
    | Rewrite of term * term * proof * proof
    | Apply of T.term * proof * link list * proof
    | Intro of link list * proof
    | Induction of T.term * proof * proof * T.var * proof

  and link = Lvar of var | Lterm of term

  exception Incomplete   (* proof is rejected : it doesn't entail goal *)
  exception Inconsistent (* proof is rejected : it proves false *)

  let rec link xs thm links =
    match T.repr thm , links with
      | _ , [] -> []
      | Bind(Forall,_,p) , Lterm t :: links -> 
	  t :: link xs p links
      | Bind(Exists,_,p) , Lvar x :: links when not (Vars.mem x xs) ->
	  e_var x :: link (Vars.add x xs) p links
      | _ ->
	  raise Inconsistent

  let rec intro env goal links =
    match T.repr goal , links with
      | _ , [] -> env,goal
      | Bind(Forall,x,p) , Lvar y :: links ->
	  if Vars.mem y env.domain then raise Inconsistent ;
	  let env = { env with domain = Vars.add y env.domain } in
	  let goal = e_subst x (e_var y) p in
	  intro env goal links
      | Bind(Exists,x,p) , Lterm t :: links ->
	  let goal = e_subst x t p in
	  intro env goal links
      | _ -> raise Inconsistent


  let rec check env goal = function

    | Compute ->
	begin
	  try
	    match check_true env goal with
	      | Yes -> ()
	      | No -> raise Inconsistent
	      | Maybe -> raise Incomplete
	  with E.Contradiction -> 
	    raise Inconsistent
	end

    | Rewrite ( a, b, proof_equal, proof ) ->
	begin
	  let h = T.e_eq a b in
	  check env h proof_equal ;
	  check_with env h (e_rewrite a b goal) proof
	end

    | Cut ( subproofs, proof ) ->
	begin
	  List.iter (fun (p,p_proof) -> check env p p_proof) subproofs ;
	  check_with_all env (List.map fst subproofs) goal proof
	end

    | Cases ( subcases , proof ) ->
	begin
	  List.iter (fun (h,h_proof) -> check_with env h goal h_proof) subcases ;
	  check env (T.e_or (List.map fst subcases)) proof
	end

    | Apply( thm, pthm, links, proof ) ->
	begin
	  check env thm pthm ;
	  let lemma = e_apply thm (link env.domain thm links) in
	  check_with env lemma goal proof
	end

    | Intro( links, proof ) ->
	
	let env,goal = intro env goal links in
	check env goal proof

    | Assumptions proof ->
	begin
	  match T.repr goal with
	    | Imply(hs,g) -> check_with_all env hs g proof
	    | _ -> check env goal proof
	end

    | Induction( measure , proof_positive , proof_init , n , proof_ind ) ->
	begin
	  let zero = T.e_int 0 in
	  check env (T.e_leq zero measure) proof_positive ;
	  check_with env (T.e_eq zero measure) goal proof_init ;
	  if Vars.mem n (T.vars goal) then raise Inconsistent ;
	  if Vars.mem n env.domain then raise Inconsistent ;
	  let next = T.e_add (T.e_var n) (T.e_int 1) in
	  let m_pos = T.e_leq zero measure in
	  let m_max = T.e_leq measure (T.e_var n) in
	  let h_ind = T.e_imply [m_pos;m_max] goal in
	  let xs = Vars.elements (Vars.union (T.vars goal) (T.vars measure)) in
	  check_with_all env
	    [ (* Hind *) T.e_forall xs h_ind ;
	      (*Ha*) T.e_eq measure next ;
	    ] goal proof_ind ;
	end

  and check_with env hyp goal proof =
    match assume env hyp with
      | Some env -> check env goal proof
      | None -> ()

  and check_with_all env hypotheses goal proof =
    match assume_all env hypotheses with
      | Some env -> check env goal proof
      | None -> ()

end
