open Logic

module Make(T : Term) =
struct

  open T

  type range = {
    value : T.term ;
    inf : Tset.t ;
    sup : Tset.t ;
    min : Z.t option ;
    max : Z.t option ;
  }

  let pp_set fmt xs =
    let first = ref true in
    Tset.iter
      (fun x ->
	 let k = T.id x in
	 if !first 
	 then ( Format.fprintf fmt "%d" k ; first := false )
	 else Format.fprintf fmt ",%d" k)
      xs

  let pp_opt fmt = function
    | None -> Format.fprintf fmt "-"
    | Some z -> Z.pretty fmt z

  let pp_range fmt r =
    begin
      Format.fprintf fmt "Domain %d:@\n" (T.id r.value) ;
      Format.fprintf fmt "  Value @[%a@]@\n" T.pretty r.value ;
      Format.fprintf fmt "  Range [%a..%a]@\n" pp_opt r.min pp_opt r.max ;
      Format.fprintf fmt "  Inf @[%a@]@\n" pp_set r.inf ;
      Format.fprintf fmt "  Sup @[%a@]@\n" pp_set r.sup ;
    end

  let pretty fmt domain =
    Tmap.iter (fun _ -> pp_range fmt) domain

  type eqs = term Simplifier.eqs
  type domain = range Tmap.t

  let top x = { value=x ; inf=Tset.empty ; sup=Tset.empty ; min=None ; max=None }

  let subset d0 d1 =
    Tset.subset d1.inf d0.inf && Tset.subset d1.sup d0.sup

  let bigunion s f xs = Tset.fold (fun x s -> Tset.union (f x) s) xs s

  let take_max a b = Z.lt a b
  let take_min a b = Z.lt b a

  let best f xs =
    Tset.fold
      (fun e m ->
	 match m , T.repr e with
	   | None , Kint k -> Some k
	   | Some k0 , Kint k when f k0 k -> Some k
	   | _ -> m) 
      xs None

  let range (eqs:eqs) e inf sup =
    let min = best take_max inf in
    let max = best take_min sup in
    let v = match min , max with
      | Some a , Some b when Z.lt b a -> raise Simplifier.NoSolution 
      | Some a , Some b when Z.leq b a -> T.e_z a
      | _ -> e in
    let xs = Tset.inter inf sup in
    Tset.iter (eqs#merge v) xs ;
    { value=v ; inf=inf ; sup=sup ; min=min ; max=max }
      
  let are_leq za zb =
    match za , zb with
      | Some a , Some b -> Z.leq a b
      | _ -> false

  let are_lt za zb =
    match za , zb with
      | Some a , Some b -> Z.lt a b
      | _ -> false

  let compute_leq (domain:domain) a b =
    let da = try Tmap.find a domain with Not_found -> top a in
    let db = try Tmap.find b domain with Not_found -> top b in
    if are_leq da.max db.min then T.e_true else
      if are_lt db.max da.min then T.e_false else
	if Tset.mem a db.inf then T.e_true else
	  if Tset.mem a db.sup then T.e_eq a b else
	    if Tset.mem b da.sup then T.e_true else
	      if Tset.mem b da.inf then T.e_eq a b else
		T.e_leq a b

  let compute_lt (domain:domain) a b =
    let da = try Tmap.find a domain with Not_found -> top a in
    let db = try Tmap.find b domain with Not_found -> top b in
    if are_lt da.max db.min then T.e_true else
      if are_leq db.max da.min then T.e_false else
	if Tset.mem a db.inf then T.e_true else
	  if Tset.mem a db.sup then T.e_false else
	    if Tset.mem b da.sup then T.e_true else
	      if Tset.mem b da.inf then T.e_false else
		T.e_lt a b

  let fixpoint (eqs:eqs) (domain:domain) : domain =
    let modified = ref true in
    let domain = ref domain in
    let get_inf x = try (Tmap.find x !domain).inf with Not_found -> Tset.empty in
    let get_sup x = try (Tmap.find x !domain).sup with Not_found -> Tset.empty in
    while !modified do
      modified := false ;
      Tmap.iter
	(fun _ d ->
	   let inf = bigunion d.inf get_inf d.inf in
	   let sup = bigunion d.sup get_sup d.sup in
	   let d' = range eqs d.value inf sup in
	   if not (subset d' d) then 
	     ( domain := Tmap.add d.value d' !domain ; modified := true )
	) !domain
    done ; !domain

  let assume_leq (eqs:eqs) (domain:domain) a b =
    let da = try Tmap.find a domain with Not_found -> top a in
    let db = try Tmap.find b domain with Not_found -> top b in
    let da' = range eqs a da.inf (Tset.add b (Tset.union db.sup da.sup)) in
    let db' = range eqs b (Tset.add a (Tset.union da.inf db.inf)) db.sup in
    Tmap.add a da' (Tmap.add b db' domain)

  let assume (eqs:eqs) (domain:domain) h =
    match T.repr h with
      | Leq(a,b) -> 
	  assume_leq eqs domain a b
      | Lt(a,b)  -> 
	  let domain = assume_leq eqs domain a b in
	  begin
	    match T.sort a , T.sort b with
	      | Sint , Sint -> assume_leq eqs domain (e_int 1) (e_sub b a)
	      | _ -> domain
	  end
      | _ -> domain

  let normalize (eqs:eqs) (domain:domain) =
    Tmap.fold
      (fun _ d domain ->
	 let e = eqs#repr d.value in
	 let d0 = try Tmap.find e domain with Not_found -> top e in
	 let inf = Tset.map eqs#repr d.inf in
	 let sup = Tset.map eqs#repr d.sup in
	 let d1 = range eqs e (Tset.union d0.inf inf) (Tset.union d0.sup sup) in
	 Tmap.add e d1 domain
      ) domain Tmap.empty

  class simplifier rg0 =
  object

    val mutable domain : domain = rg0

    method copy = ( new simplifier domain :> term Simplifier.t )

    method compute (_ : term Simplifier.eqs) e = 
      match T.repr e with
	| Leq(a,b) -> compute_leq domain a b
	| Lt(a,b) -> compute_lt domain a b
	| _ -> e

    method assume (eqs : term Simplifier.eqs) h = 
      domain <- assume eqs domain h

    method normalize (eqs : term Simplifier.eqs) =
      domain <- fixpoint eqs (normalize eqs domain)

  end

  let simplifier () = new simplifier Tmap.empty

end

