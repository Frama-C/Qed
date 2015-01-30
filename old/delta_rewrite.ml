(* ------------------------------------------------------------------------ *)
(* ---  Simplifier based on rewriting rules                             --- *)
(* ------------------------------------------------------------------------ *)

open Logic

module Make(D : Delta.S) =
struct

  module T = D.T

  type redex = int * T.f * cpattern list
  and cpattern =
    | Cany
    | Cvar of int
    | Cint of Z.t
    | Ctrue
    | Cfalse
    | Credex of redex
  and rewrite = (T.f,T.term) Logic.rewrite * int list array

  module Fmap = Map.Make(T.Fun)

  type env = {
    mutable cpattern : int ;
    mutable redex : redex list Fmap.t ;
    mutable rules : rewrite list Intmap.t ;
  }

  type cmatch =
    | Partial
    | Mismatch
    | Match of T.term array

  (* ------------------------------------------------------------------------ *)
  (* --- Patterns Compilation                                             --- *)
  (* ------------------------------------------------------------------------ *)

  let rec cpattern env k vars = function
    | Pany -> Cany
    | Ptrue -> Ctrue
    | Pfalse -> Cfalse
    | Pint z -> Cint z
    | Pvar x ->
	let r = !k in incr k ; 
	vars.(x) <- r :: vars.(x) ; 
	Cvar r
    | Pfun _ as p -> Credex (credex env k vars p)

  and credex env k vars = function
    | Pany | Ptrue | Pfalse | Pint _ | Pvar _ -> assert false
    | Pfun(f,ps) ->
	let r = env.cpattern in 
	env.cpattern <- succ r ;
	let redex = r,f,List.map (cpattern env k vars) ps in
	let redxs = try Fmap.find f env.redex with Not_found -> [] in 
	env.redex <- Fmap.add f (redex::redxs) env.redex ;
	redex
	  
  let redex_id ((k,_,_):redex) = k

  let crule env rw =
    let vars = Array.create rw.pvars [] in
    let redex = credex env (ref 0) vars rw.pattern in
    let rid = redex_id redex in
    let rules = try Intmap.find rid env.rules with Not_found -> [] in
    env.rules <- Intmap.add (redex_id redex) ((rw,vars)::rules) env.rules
	  
  (* ------------------------------------------------------------------------ *)
  (* --- Partial Substitutions                                            --- *)
  (* ------------------------------------------------------------------------ *)

  module Sigma =
  struct

    type t = T.term option array

    let empty : t = [| |]

    let merge xs ys : t option =
      let n = Array.length xs in
      if n = 0 then Some ys else
	let m = Array.length ys in
	if m = 0 then Some xs else
	  let k = max n m in
	  let zs = Array.create k None in
	  try
	    for i = 0 to k-1 do
	      let x = if i < n then xs.(i) else None in
	      let y = if i < m then ys.(i) else None in
	      match x,y with
		| Some _,Some _ -> raise Exit
		| Some _,None -> zs.(i) <- x
		| None,Some _ -> zs.(i) <- y
		| None,None -> ()
	    done ; Some zs
	  with Exit -> None

    let bind i x : t =
      let e = Array.create (succ i) None in
      e.(i) <- Some x ; e

    let map f es : t = Array.map (function Some e -> Some (f e) | None -> None) es

    exception Cmp of int

    let compare xs ys =
      let n = Array.length xs in
      let m = Array.length ys in
      if n < m then (-1) else
	if n > m then 1 else
	  try
	    for i = 0 to n-1 do
	      match xs.(i) , ys.(i) with
		| None,None -> ()
		| Some _,None -> raise (Cmp 1)
		| None,Some _ -> raise (Cmp (-1))
		| Some x,Some y ->
		    let k = T.compare x y in
		    if k <> 0 then raise (Cmp k)
	    done ; 0
	  with Cmp k -> k

    exception EMismatch
    exception EPartial
	    
    let rec bind_same delta s e = function
      | [] -> e
      | k::ks -> 
	  if k >= Array.length s then raise EPartial ;
	  match s.(k) with
	    | None -> raise EPartial
	    | Some e0 ->  
		match D.compare delta e e0 with
		  | Delta.NotEqual -> raise EMismatch
		  | Delta.Compare _ -> raise EPartial
		  | Delta.Equal e' -> bind_same delta s e' ks
		
    let bind delta s = function
      | [] -> raise EPartial
      | k::ks -> 
	  if k >= Array.length s then raise EPartial ;
	  match s.(k) with
	    | None -> raise EPartial
	    | Some e -> bind_same delta s e ks

    let matching delta (s:t) (bindings:int list array) : cmatch = 
      try Match(Array.map (bind delta s) bindings)
      with EPartial -> Partial | EMismatch -> Mismatch

  end

  module Matches = Set.Make(Sigma)

  (* ------------------------------------------------------------------------ *)
  (* --- Matching                                                         --- *)
  (* ------------------------------------------------------------------------ *)

  module Cinfo =
  struct

    type t = {
      mutable parents : T.term list ;
      mutable matches : Matches.t Intmap.t ;
    }

  end

end
