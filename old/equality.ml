(* -------------------------------------------------------------------------- *)
(** Equivalence Classes                                                       *)
(* -------------------------------------------------------------------------- *)

open Logic

module Make(T : Term) =
struct

  open T

  type eqs = {
    mutable dag : term Tmap.t ; (* union-find management *)
    mutable citizen : Tset.t Tmap.t ;  (* class-eq *)
    mutable parents : Tset.t Tmap.t ;  (* parents *)
    mutable rebuild : Tset.t ; (* classes to rebuild *)
    mutable pending : ( term * term ) list ; (* classes to merge *)
  }

  exception Contradiction

  (* -------------------------------------------------------------------------- *)
  (* --- Equivalence Class Management                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rec union_find eqs t =
    try
      let t0 = Tmap.find t eqs.dag in
      let t1 = union_find eqs t0 in
      if t1 != t0 then eqs.dag <- Tmap.add t t1 eqs.dag ; t1
    with Not_found -> t

  let check_equal a b =
    match T.is_true (T.e_eq a b) with
      | Yes | Maybe -> ()
      | No -> raise Contradiction

  let r_parents eqs r =
    try Tmap.find r eqs.parents
    with Not_found -> Tset.empty

  (* r is parent of e *)
  let link_parent eqs r e =
    (* small optimization with direct cycle. 
       Not necessary to ensure termination. *)
    if e!=r then
      let parents = r_parents eqs e in
      if not (Tset.mem r parents) then
	eqs.parents <- Tmap.add e (Tset.add r parents) eqs.parents

  let r_citizen eqs r =
    try Tmap.find r eqs.citizen
    with Not_found -> 
      T.e_iter (link_parent eqs r) r ;
      let ceq = Tset.singleton r in
      eqs.citizen <- Tmap.add r ceq eqs.citizen ; ceq

  let merge_term eqs a b =
    let a = union_find eqs a in
    let b = union_find eqs b in
    if a != b then
      begin
	check_equal a b ;
	let r,s = if T.compare a b <= 0 then a,b else b,a in
	let citizen = Tset.union (r_citizen eqs a) (r_citizen eqs b) in
	let parents = Tset.union (r_parents eqs a) (r_parents eqs b) in
	eqs.rebuild <- Tset.union parents eqs.rebuild ;
	eqs.citizen <- Tmap.add r citizen eqs.citizen ;
	eqs.citizen <- Tmap.remove s eqs.citizen ;
	eqs.dag <- Tmap.add s r eqs.dag ;
      end

  let rec add_hypothesis eqs p =
    match T.repr p with
      | True -> ()
      | False -> raise Contradiction
      | And hs -> List.iter (add_hypothesis eqs) hs
      | Eq(a,b) -> merge_term eqs a b
      | _ -> 
	  merge_term eqs p T.e_true ;
	  merge_term eqs (T.e_not p) T.e_false

  (* -------------------------------------------------------------------------- *)
  (* --- Saturation                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  (* new citizen [t] in class [a] *)
  let add_citizen eqs a t =
    let b = union_find eqs t in
    if a!=b then 
      begin
	check_equal a b ;
	eqs.pending <- (b,a) :: eqs.pending ;
      end

  (* infer new equalities in True *)
  let rec add_true eqs p =
    match T.repr p with
      | True -> ()
      | False -> raise Contradiction
      | And hs -> List.iter (add_true eqs) hs
      | Eq(a,b) -> eqs.pending <- (a,b) :: eqs.pending
      | Not p -> add_false eqs p
      | _ -> ()

  (* infer new equalities in False *)
  and add_false eqs p =
    match T.repr p with
      | False -> ()
      | True -> raise Contradiction
      | Or hs -> List.iter (add_false eqs) hs
      | Neq(a,b) -> eqs.pending <- (a,b) :: eqs.pending
      | Not p -> add_true eqs p
      | _ -> ()

  (* infer new equalities when class [a] has citizen [b] *)
  let add_property eqs a b =
    match T.repr a with
      | True  -> add_true  eqs b
      | False -> add_false eqs b
      | _ -> ()

  let rec fix_term eqs xs a =
    match T.repr a with

      | Bind(q,x,e) -> 
	  e_bind q x (fix_term eqs (Vars.add x xs) e)
	      
      | _ ->
	  if Vars.intersect (T.vars a) xs
	  then T.e_map (fix_term eqs xs) a
	  else T.e_map (union_find eqs) a

  (* recompute class [a] and infer new equalities *)
  let fix_class eqs a =
    let a = union_find eqs a in
    let rs = Tset.map (union_find eqs) (r_parents eqs a) in
    let xs = Tset.map
      (fun e -> 
	 let r = fix_term eqs Vars.empty e in
	 add_citizen eqs a r ;
	 add_property eqs a r ;
	 r)
      (r_citizen eqs a)
    in
    begin
      eqs.citizen <- Tmap.add a xs eqs.citizen ;
      eqs.parents <- Tmap.add a rs eqs.parents ;
    end

  (* -------------------------------------------------------------------------- *)
  (* --- Fixpoint                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let is_fixpoint eqs =
    Tset.is_empty eqs.rebuild && eqs.pending = []

  let fix eqs =
    while not (is_fixpoint eqs) do
      let pending = eqs.pending in
      if pending <> [] then
	begin
	  eqs.pending <- [] ; 
	  List.iter (fun (a,b) -> merge_term eqs a b) pending ;
	end ;
      let rebuild = eqs.rebuild in
      if not (Tset.is_empty rebuild) then
	begin
	  eqs.rebuild <- Tset.empty ; 
	  Tset.iter (fix_class eqs) rebuild ;
	end
    done

  (* -------------------------------------------------------------------------- *)
  (* --- Module API                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let class_of = r_citizen
  let repr_of  = union_find

  exception Skip

  let iter f eqs = 
    Tmap.iter 
      (fun _ xs -> 
	 try Tset.iter (fun x -> f (union_find eqs x) xs ; raise Skip) xs
	 with Skip -> ()
      ) eqs.citizen

  let identity = {
    dag = Tmap.empty ;
    citizen = Tmap.empty ;
    parents = Tmap.empty ;
    rebuild = Tset.empty ;
    pending = [] ;
  }

  let copy eqs = 
    fix eqs ; {
      dag = eqs.dag ;
      citizen = eqs.citizen ;
      parents = eqs.parents ;
      rebuild = Tset.empty ;
      pending = [] ;
    }

  let compute eqs a = fix_term eqs Vars.empty a

  let merge eqs a b =
    let a = union_find eqs a in
    let b = union_find eqs b in
    if a==b then eqs else
      let s = copy eqs in 
      merge_term s a b ; fix s ; s
	
  let assume eqs p =
    let s = copy eqs in
    add_hypothesis s p ; fix s ; s

  let are_equal eqs a b = T.is_true (compute eqs (T.e_eq a b))
      
end
