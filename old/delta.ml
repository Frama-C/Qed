(* ------------------------------------------------------------------------ *)
(* ---  Proof Environments                                              --- *)
(* ------------------------------------------------------------------------ *)

open Logic

type 'a comparison =
  | NotEqual
  | Equal of 'a
  | Compare of 'a * 'a

module type S =
sig

  type env

  module T : Logic.Term

  exception Contradiction

  class type simplifier =
  object
    method copy     : simplifier
    method assumed  : env -> T.Tset.t -> unit
    method reduced : env -> T.Tset.t -> unit
    method simplify : env -> T.term -> T.term
  end

  val create   : T.pool -> env
  val copy     : env -> env
  val class_of : env -> T.term -> T.Tset.t
  val repr_of  : env -> T.term -> T.term
  val compare  : env -> T.term -> T.term -> T.term comparison
  val merge    : env -> T.term -> T.term -> unit
  val assume   : env -> T.term -> unit
  val compute  : env -> T.term -> T.term array -> T.term
  val simplify : env -> T.term -> T.term
  val register : env -> simplifier -> unit

end

module Make (T : Logic.Term) =
struct
  
  module T = T
  open T

  exception Contradiction

  type action = 
    | Merge of term * term
    | Assume of term

  type env = {
    pool : T.pool ;
    mutable dag : term Tmap.t ; (* union-find for equal terms *)
    mutable ceq : Tset.t Tmap.t ;  (* set of representatives *)
    mutable demon : demon list ;
    mutable pending : action list ;
    mutable lock : bool ; (* currently merging terms *)
    mutable reduced : Tset.t ; (* set or recently reduced terms *)
    mutable assumed : Tset.t ;  (* set of recently assumed terms *)
  } and demon = 
  < 
    copy : demon ;
    assumed : env -> Tset.t -> unit ;
    reduced : env -> Tset.t -> unit ;
    simplify : env -> term -> term ;
  >

  class type simplifier =
  object
    method copy     : simplifier
    method assumed  : env -> Tset.t -> unit
    method reduced : env -> Tset.t -> unit
    method simplify : env -> term -> term
  end

  let create pool = {
    pool = pool ;
    dag = Tmap.empty ;
    ceq = Tmap.empty ;
    demon = [] ;
    pending = [] ;
    lock = false ;
    reduced = Tset.empty ;
    assumed = Tset.empty ;
  }

  let copy env =
    assert ( env.pending = [] && not env.lock ) ;
    let env' = { 
      pool = env.pool ; dag = env.dag ; ceq = env.ceq ;
      pending = [] ; lock = false ; demon = [] ;
      reduced = Tset.empty ;
      assumed = Tset.empty ;
    } in
    env'.demon <- List.map (fun d -> d#copy) env.demon ; env'

  let register env d =
    env.demon <- env.demon @ [d]

  (* ------------------------------------------------------------------------ *)
  (* --- Dag Management                                                   --- *)
  (* ------------------------------------------------------------------------ *)
      
  let rec union_find env e =
    try
      let e0 = Tmap.find e env.dag in
      let e1 = union_find env e0 in
      if e1 != e0 then env.dag <- Tmap.add e e1 env.dag ; e1
    with Not_found -> e

  let repr_of env e = union_find env e
  let class_of env e =
    try Tmap.find (union_find env e) env.ceq
    with Not_found -> Tset.singleton e

  let compare env a b =
    let a = union_find env a in
    let b = union_find env b in
    if a == b then Equal a
    else
      let e = union_find env (T.e_neq a b) in
      match T.maybe e with
	| Yes -> NotEqual
	| No -> Equal a
	| Maybe -> Compare(a,b)

  (* Requires e == union_find e *)
  let get_ceq env e = 
    try Tmap.find e env.ceq with Not_found -> Tset.singleton e

  (* Requires a,b == union_find of them, and a>b *)
  let do_fusion env a b =
    let ca = get_ceq env a in
    let cb = get_ceq env b in
    let ce = Tset.union ca cb in
    begin
      env.ceq <- Tmap.remove a env.ceq ;
      env.ceq <- Tmap.add b ce env.ceq ;
      env.dag <- Tmap.add a b env.dag ;
      match T.maybe b with
	| Logic.Yes -> env.assumed <- Tset.add a env.assumed
	| Logic.No -> env.assumed <- Tset.add (T.e_not a) env.assumed
	| Logic.Maybe -> env.reduced <- Tset.add a env.reduced
    end

  (* Requires b if true or false *)
  let do_property env a b =
    if a != b then
      let ca = get_ceq env a in
      let cb = get_ceq env b in
      let ce = Tset.union ca cb in
      begin
	env.ceq <- Tmap.remove a env.ceq ;
	env.ceq <- Tmap.add b ce env.ceq ;
	env.dag <- Tmap.add a b env.dag ;
      end

  let do_merge env a b =
    if a!=b then
      let a0 = union_find env a in
      let b0 = union_find env b in
      let cmp = T.compare a0 b0 in
      if cmp>0 then do_fusion env a0 b0 else
	if cmp<0 then do_fusion env b0 a0

  let rec do_assume env e =
    match T.repr e with
      | True -> ()
      | False -> raise Contradiction
      | And es -> List.iter (do_assume env) es
      | Eq(a,b) -> do_merge env a b
      | _ -> 
	  let a = union_find env e in
	  let b = union_find env (T.e_not a) in
	  if a == b then raise Contradiction ;
	  do_property env a T.e_true ; 
	  do_property env b T.e_false ;
	  env.assumed <- Tset.add a env.assumed

  (* ------------------------------------------------------------------------ *)
  (* --- Fixpoint                                                         --- *)
  (* ------------------------------------------------------------------------ *)

  let process env =
    while env.pending <> [] do
      List.iter 
	(function
	   | Merge(a,b) -> do_merge env a b
	   | Assume e -> do_assume env e
	) env.pending ;
      try
	env.pending <- [] ;
	env.lock <- true ;
	List.iter
	  (fun d ->
	     d#reduced env env.reduced ;
	     d#assumed env env.assumed ;
	  ) env.demon ;
	env.reduced <- Tset.empty ;
	env.assumed <- Tset.empty ;
	env.lock <- false ;
      with error ->
	env.lock <- false ;
	raise error ;
    done

  let merge env a b =
    let a0 = union_find env a in
    let b0 = union_find env b in
    if a0 != b0 then 
      ( env.pending <- Merge(a,b) :: env.pending ;
	if not env.lock then process env )
      
  let assume env e =
    let e = union_find env e in
    match T.repr e with
      | True -> ()
      | False -> raise Contradiction
      | _ -> 
	  ( env.pending <- Assume e :: env.pending ;
	    if not env.lock then process env )

  (* ------------------------------------------------------------------------ *)
  (* --- Computation                                                      --- *)
  (* ------------------------------------------------------------------------ *)

  (* apply simplifiers *)
  let rec apply env e0 e = function
    | [] -> if e==e0 then e else apply env e e env.demon
    | d::ds -> apply env e0 (union_find env (d#simplify env e)) ds
	
  (* requires sigma to be already computed *)
  let rec do_compute memoized env sigma e =
    let e = union_find env e in
    match T.repr e with
      | Kint _ | Kreal _ | True | False -> e
      | Bvar(k,_) -> sigma.(k)
      | _ ->
	  try Tmap.find e !memoized with Not_found ->
	    let e0 = T.e_map ~closure:false (do_compute memoized env sigma) e in
	    let e1 = union_find env e0 in
	    let e2 = apply env e1 e1 env.demon in
	    memoized := Tmap.add e e2 !memoized ; 
	    do_merge env e e2 ; e2

  let compute env e sigma = do_compute (ref Tmap.empty) env sigma e

  (* ------------------------------------------------------------------------ *)
  (* --- Interface                                                        --- *)
  (* ------------------------------------------------------------------------ *)

  let perform env action =
    env.pending <- action :: env.pending ;
    if not env.lock then process env

  let assume env e =
    let h = compute env e [| |] in
    if h != T.e_true then perform env (Assume h)

  let merge env a b =
    let a = compute env a [| |] in
    let b = compute env b [| |] in
    let c = T.compare a b in
    if c<0 then
      perform env (Merge(b,a))
    else if c>0 then
      perform env (Merge(a,b))

  (* ------------------------------------------------------------------------ *)
  (* --- Simplifier                                                       --- *)
  (* ------------------------------------------------------------------------ *)

  let rec simplify env e =
    process env ; do_simplify env (compute env e [| |])

  and do_simplify env e = 
    match T.repr e with

      | Imply(hs,p) ->
	  
	  let hs = Array.of_list hs in
	  do_conjunction env hs ;
	  let env' = copy env in
	  begin
	    try
	      Array.iter (do_assume env') hs ;
	      let q = simplify env' p in
	      T.e_imply (Array.to_list hs) q
	    with Contradiction -> T.e_true
	  end

      | And ps ->

	  let ps = Array.of_list ps in
	  do_conjunction env ps ;
	  T.e_and (Array.to_list ps)
    
      | Apply(phi,es) ->
	  
	  let binders = ref [] in
	  let sigma =
	    Array.mapi
	      (fun i (q,t) ->
		 if i < Array.length es then 
		   es.(i) 
		 else
		   let x = T.fresh env.pool t in
		   (binders := (q,x) :: !binders ; T.e_var x)
	      ) (T.f_prm phi)
	  in
	  let e = do_simplify env (compute env (T.f_def phi) sigma) in
	  List.fold_left
	    (fun e (q,x) ->
	       T.e_bind q x e
	    ) e !binders

      | Or _ | Not _ -> T.e_map ~closure:false (do_simplify env) e
      | _ -> e

  and do_conjunction env hs =
    Array.iteri
      (fun i h ->
	 let env' = copy env in
	 try
	   hs.(i) <- T.e_true ;
	   Array.iter (do_assume env') hs ;
	   hs.(i) <- simplify env' h ;
	 with Contradiction -> 
	   hs.(i) <- T.e_false
      ) hs
      
end
