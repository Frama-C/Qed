(* ------------------------------------------------------------------------ *)
(* ---  Simplifier                                                      --- *)
(* ------------------------------------------------------------------------ *)


  (* --- Affine Forms --- *)
(*
  let affine e = match e.repr with
    | Add ts ->  
	List.map
	  (fun e ->
	     match e.repr with
	       | Times(k,e) -> k,e
	       | Kint k -> k,e_one
	       | _ -> Z.one , e
	  ) ts
    | Times(k,e) -> [ k , e ]
    | Kint k -> [ k , e_one ]
    | _ -> [ Z.one , e ]
*)

module Make(S : Delta.S)(D : Numerical.Domain) =
struct

  module T = S.T

  (* Each class eq of S.env is assigned a constrained variable *)
    
  type equation =
    | Sum of T.term * ( Z.t * T.term ) array (* x = Sum ki.xi *)
    | Mul of T.term * T.term list        (* x = Prod yi *)
    | Div of T.term * T.term * T.term    (* x = a dib b *) 
    | Mod of T.term * T.term * T.term    (* x = a mod b *)

  type system = {
    mutable dom : D.d T.Tmap.t ;  (* Domain for Variables *)
    mutable eqs : equation list ; (* Constraints *)
    mutable vars : T.Tset.t ;     (* Constrained Variables *)
    mutable singles : T.Tset.t ;  (* Singleton that just appeared *)
    mutable modified : bool ;     (* A domain has been reduced *)
  }

  let eqvars xs = function
    | Sum(x,kxs) -> 
	Array.fold_left 
	  (fun xs (_,y) -> T.Tset.add y xs) 
	  (T.Tset.add x xs) kxs
    | Mul(x,es) -> 
	List.fold_left 
	  (fun xs y -> T.Tset.add y xs) 
	  xs (x::es)
    | Div(x,a,b) | Mod(x,a,b) -> 
	T.Tset.add a (T.Tset.add b (T.Tset.add x xs))

  let pp_var fmt e = Format.fprintf fmt "X%d" (T.id e)

  let pp_eq fmt = function
    | Sum(x,kxs) ->
	Format.fprintf fmt "%a = S" pp_var x ;
	Array.iter
	  (fun (k,x) ->
	     if Z.positive k 
	     then Format.fprintf fmt " +%a.%a" Z.pretty k pp_var x
	     else Format.fprintf fmt " %a.%a" Z.pretty k pp_var x)
	  kxs ;
    | Mul(x,xs) ->
	Format.fprintf fmt "%a = P" pp_var x ;
	List.iter (fun y -> Format.fprintf fmt ".%a" pp_var y) xs
    | Div(x,a,b) ->
	Format.fprintf fmt "%a = %a DIV %a" pp_var x pp_var a pp_var b
    | Mod(x,a,b) ->
	Format.fprintf fmt "%a = %a MOD %a" pp_var x pp_var a pp_var b

  let pretty fmt s =
    begin
      T.Tmap.iter
	(fun k d ->
	   Format.fprintf fmt "@[X%d in %a@]@\n" k D.pretty d) 
	s.dom ;
      List.iter
	(fun e ->
	   Format.fprintf fmt "@[%a@]@\n" pp_eq e)
	s.eqs ;
      T.Tset.iter
	(fun e ->
	   Format.fprintf fmt "@[%a = %a@]@\n" pp_var e T.pretty e
	) s.vars ;
      Format.pp_print_flush fmt () ;
    end

  (* ------------------------------------------------------------------------ *)
  (* --- Domain Cut                                                       --- *)
  (* ------------------------------------------------------------------------ *)

  let check_single s x d =
    match D.singleton d with
      | None -> ()
      | Some _ -> s.singles <- T.Tset.add x s.singles

  let get s x = try T.Tmap.find x s.dom with Not_found -> D.top
  let set s x d =
    try
      let d0 = T.Tmap.find x s.dom in
      if D.subset d0 d then d0
      else
	let d1 = D.cap d0 d in
	if D.is_empty d1 then raise S.Contradiction 
	else
	  ( s.modified <- true ;
	    s.dom <- T.Tmap.add x d1 s.dom ; 
	    check_single s x d1 ; d1 )
    with Not_found ->
      if D.is_empty d then raise S.Contradiction ;
      s.modified <- true ;
      s.dom <- T.Tmap.add x d s.dom ; 
      check_single s x d ; d

  let cut s x d = ignore (set s x d)

  (* ------------------------------------------------------------------------ *)
  (* --- Propagation Analysis                                             --- *)
  (* ------------------------------------------------------------------------ *)

  let delta ds d i =
    let r = ref d in
    for j=0 to Array.length ds - 1 do
      if i<>j then r := D.add !r ds.(i)
    done ; !r

  let sum s kxs =
    let r = ref (D.int Z.zero) in
    for i=0 to Array.length kxs - 1 do
      let k,x = kxs.(i) in
      r := D.add !r (D.times k (get s x))
    done ; !r

  let rec prod s = function
    | [] -> D.int Z.one
    | x::xs -> 
	List.fold_left
	  (fun d y -> D.mul d (get s y))
	  (get s x) xs

  let propagate (s:system) = function
    | Sum(x,kys) ->	  
	let d0 = sum s kys in
	let d1 = D.cap d0 (get s x) in
	let ds = Array.map (fun (k,x) -> D.times (Z.opp k) (get s x)) kys in
	Array.iteri
	  (fun i (k,y) ->
	     cut s y (D.divided (delta ds d1 i) k)) 
	  kys ;
	cut s x (D.cap d1 (sum s kys))
	
    | Mul(x,xs) ->
	cut s x (prod s xs)

    | _ -> ()


(*
      | T.Div(a,b) ->
	  ( match D.singleton (get s b) with
	      | None -> D.top
	      | Some k -> D.divided (get s a) k )

      | T.Mod(_,b) ->
	  ( match D.singleton (get s b) with
	      | None -> D.top
	      | Some k ->
		  if Z.positive k 
		  then D.range Z.zero (Z.add k Z.minus_one)
		  else D.range (Z.add k Z.one) Z.zero )
*)

  (* ------------------------------------------------------------------------ *)
  (* --- Simplification Routine                                           --- *)
  (* ------------------------------------------------------------------------ *)
	   
end
