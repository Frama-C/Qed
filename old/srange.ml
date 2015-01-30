open Logic

module type Domain =
sig

  type d
  val top : d
  val cap : d -> d -> d
  val subset : d -> d -> bool
  val is_top : d -> bool
  val is_empty : d -> bool
  val is_singleton : d -> Z.t option
  val singleton : Z.t -> d

  val opp : d -> d
  val add : d -> d -> d
  val mul : d -> d -> d
  val times : Z.t -> d -> d

  val inf_to : d -> d (* the set of y <= x with x in d *)
  val sup_to : d -> d (* the set of y >= x with x in d *)

  val are_leq : d -> d -> Logic.maybe (* x <= y forall x in d1, y in d2 *)
  val are_lt  : d -> d -> Logic.maybe (* x < y forall x in d1, y in d2 *)

  val pretty : Format.formatter -> d -> unit

end

module Interval : Domain = Range

module Make(T : Term)(D : Domain) =
struct

  open T

  type eqs = (term -> unit)

  let have_equal (eqs:eqs) a b = eqs (T.e_eq a b)

  type range = {
    value : term ;
    domain : D.d ;
    inf : Tset.t ; (* set of terms <= value *)
    sup : Tset.t ; (* set of terms >= value *)
  }

  type dmap = {
    mutable modified : bool ; (* inf+sup should be saturated *)
    mutable range : range Tmap.t ; (* for non-top *)
  }

  exception EmptyDomain

  (* -------------------------------------------------------------------------- *)
  (* --- Utilities                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let d_zero = D.singleton Z.zero
  let d_one = D.singleton Z.one

  let pp_set fmt xs =
    let first = ref true in
    Tset.iter
      (fun x ->
	 let k = T.id x in
	 if !first 
	 then ( Format.fprintf fmt "%d" k ; first := false )
	 else Format.fprintf fmt ",%d" k)
      xs

  let pp_range fmt r =
    begin
      Format.fprintf fmt "Range @[%a@]@\n" D.pretty r.domain ;
      Format.fprintf fmt "Inf @[%a@]@\n" pp_set r.inf ;
      Format.fprintf fmt "Sup @[%a@]@\n" pp_set r.sup ;
    end

  let pretty fmt domain =
    Tmap.iter 
      (fun id d -> 
	 Format.fprintf fmt "Domain %03d:@[<h 2>%a@]" id pp_range d)
      domain

  (* -------------------------------------------------------------------------- *)
  (* --- Fixpoint                                                           --- *)
  (* -------------------------------------------------------------------------- *)
      
  let bigunion f dmap xs =
    Tset.fold 
      (fun x s -> 
	 try Tset.union s (f (Tmap.find x dmap.range))
	 with Not_found -> s)
      xs xs

  let best f dmap xs d =
    Tset.fold
      (fun x d ->
	 try D.cap (f (Tmap.find x dmap.range).domain) d
	 with Not_found -> d
      ) xs d

  let cap v r1 r2 = {
    value = v ;
    domain = D.cap r1.domain r2.domain ;
    inf = Tset.union r1.inf r2.inf ;
    sup = Tset.union r1.sup r2.sup ;
  }

  let subset r1 r2 =
    (* r1 has less values than r2 *)
    (* r1 has more constraints than r2 *)
    begin
      D.subset r1.domain r2.domain
      && Tset.subset r2.inf r1.inf
      && Tset.subset r2.sup r1.sup
    end
      
  let check eqs r =
    begin
      if D.is_empty r.domain then raise EmptyDomain ;
      Tset.iter (have_equal eqs r.value) (Tset.inter r.inf r.sup) ;
      match D.is_singleton r.domain with 
	| None -> ()
	| Some z -> have_equal eqs r.value (T.e_z z)
    end
	
  let fixpoint dmap eqs =
    try
      while dmap.modified do
	dmap.modified <- false ;
	Tmap.iter
	  (fun _ r ->
	     let d_sup = best D.inf_to dmap r.sup r.domain in
	     let d_inf = best D.sup_to dmap r.inf r.domain in
	     let r_sup = bigunion (fun x -> x.sup) dmap r.sup in
	     let r_inf = bigunion (fun x -> x.inf) dmap r.inf in
	     let r' = { 
	       value=r.value ; 
	       domain = D.cap d_sup d_inf ;
	       inf = r_inf ;
	       sup = r_sup ;
	     } in
	     if not (subset r' r) then
	       ( check eqs r' ;
		 dmap.modified <- true ;
		 dmap.range <- Tmap.add r'.value r' dmap.range )
	  ) dmap.range ;
      done
    with EmptyDomain ->
      dmap.modified <- false ; eqs e_false

  (* -------------------------------------------------------------------------- *)
  (* --- Domain Update                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let assume_range dmap eqs e r =
    try
      try
	let r0 = Tmap.find e dmap.range in
	if not (subset r r0) then
	  begin
	    let r1 = cap e r0 r in
	    check eqs r1 ;
	    dmap.modified <- true ;
	    dmap.range <- Tmap.add e r1 dmap.range ;
	  end
      with Not_found ->
	check eqs r ;
	dmap.modified <- true ;
	dmap.range <- Tmap.add e r dmap.range ;
    with EmptyDomain ->
      eqs e_false

  (* -------------------------------------------------------------------------- *)
  (* --- Domain Computation & Merge                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let rec get_range dmap e =
    try Tmap.find e dmap.range
    with Not_found ->
      let d = compute_domain dmap e in 
      let s = Tset.singleton e in 
      let r = { value=e ; domain=d ; inf=s ; sup=s } in
      dmap.range <- Tmap.add e r dmap.range ; r
	
  and compute_domain dmap e =
    match T.repr e with
      | Kint z -> D.singleton z
      | Add xs ->
	  List.fold_left
	    (fun d x -> D.add d (get_range dmap x).domain) 
	    d_zero xs
      | Mul xs -> 
	  List.fold_left
	    (fun d x -> D.mul d (get_range dmap x).domain) 
	    d_one xs
      | Times(k,a) -> D.times k (get_range dmap a).domain
      | _ -> D.top

  let smerge a b xs = Tset.map (fun x -> if x==a then b else x) xs

  let merge dmap eqs a b =
    dmap.range <- Tmap.map
      (fun _ r ->
	 if Tset.mem a r.inf || Tset.mem a r.sup then
	   { r with 
	       inf = smerge a b r.inf ;
	       sup = smerge a b r.sup }
	 else r)
      dmap.range ;
    let ra = get_range dmap a in
    let rb = get_range dmap b in
    let r = cap b ra rb in
    check eqs r ;
    dmap.range <- Tmap.add b r (Tmap.remove a dmap.range) ;
    dmap.modified <- true ;
    fixpoint dmap eqs

  (* -------------------------------------------------------------------------- *)
  (* --- Assumptions                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let inf_to r = { r with domain = D.inf_to r.domain ; inf=Tset.empty }
  let sup_to r = { r with domain = D.sup_to r.domain ; sup=Tset.empty }

  let assume_leq dmap eqs a b =
    let ra = get_range dmap a in
    let rb = get_range dmap b in
    assume_range dmap eqs a (inf_to rb) ;
    assume_range dmap eqs b (sup_to ra) ;
    fixpoint dmap eqs
    
  let assume dmap eqs h =
    match T.repr h with
      | Leq(a,b) -> 
	  assume_leq dmap eqs a b
      | Lt(a,b)  -> 
	  assume_leq dmap eqs a b ;
	  begin
	    match T.sort a , T.sort b with
	      | Sint , Sint -> assume_leq dmap eqs (e_int 1) (e_sub b a)
	      | _ -> ()
	  end
      | _ -> ()

  (* -------------------------------------------------------------------------- *)
  (* --- Simplifications                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let compute_leq dmap a b =
    let ra = get_range dmap a in
    let rb = get_range dmap b in
    match D.are_leq ra.domain rb.domain with
      | Yes -> T.e_true
      | No -> T.e_false
      | Maybe ->
	  if Tset.mem a rb.inf then T.e_true else
	    if Tset.mem a rb.sup then T.e_eq a b else
	      if Tset.mem b ra.sup then T.e_true else
		if Tset.mem b ra.inf then T.e_eq a b else
		  T.e_leq a b
		    
  let compute_lt dmap a b =
    let ra = get_range dmap a in
    let rb = get_range dmap b in
    match D.are_lt ra.domain rb.domain with
      | Yes -> T.e_true
      | No -> T.e_false
      | Maybe ->
	  if Tset.mem a rb.inf then T.e_true else
	    if Tset.mem a rb.sup then T.e_false else
	      if Tset.mem b ra.sup then T.e_true else
		if Tset.mem b ra.inf then T.e_false else
		  T.e_lt a b

  (* -------------------------------------------------------------------------- *)
  (* --- Theory                                                             --- *)
  (* -------------------------------------------------------------------------- *)


end

