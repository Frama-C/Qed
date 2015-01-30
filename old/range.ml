(* ------------------------------------------------------------------------ *)
(* ---  Interval Domain                                                 --- *)
(* ------------------------------------------------------------------------ *)

type d =
  | Top
  | Bot
  | SupTo of Z.t
  | InfTo of Z.t
  | Range of Z.t * Z.t
      
(* ------------------------------------------------------------------------ *)
(* --- Elementary Constructors                                          --- *)
(* ------------------------------------------------------------------------ *)
      
let top = Top
let bot = Bot

let is_top = function Top -> true | _ -> false
let is_empty = function Bot -> true | _ -> false
let is_singleton = function
  | Range(a,b) when Z.leq b a -> Some a
  | _ -> None

let singleton z = Range(z,z)
let range a b = if Z.leq a b then Range(a,b) else Bot
  
(* ------------------------------------------------------------------------ *)
(* --- Extended Numbers                                                 --- *)
(* ------------------------------------------------------------------------ *)
  
type e = Neg | Pos | Val of Z.t

let e_cmp p q = match p,q with
  | Neg , Neg | Pos , Pos -> 0
  | Neg , (Val _ | Pos) | Val _ , Pos -> (-1)
  | Val _ , Neg | Pos , (Val _ | Neg) -> 1
  | Val x , Val y -> Z.compare x y
      
let a_min x y = if Z.compare x y > 0 then y else x
let a_max x y = if Z.compare x y < 0 then y else x
  
let e_min p q = if e_cmp p q > 0 then q else p
let e_max p q = if e_cmp p q < 0 then q else p
  
let w_min p q = if e_cmp p q > 0 then Neg else p
let w_max p q = if e_cmp p q < 0 then Pos else p
  
let e_range a b =
  match a , b with
    | Neg , Pos -> Top
    | _ , Neg | Pos , _ -> Bot
    | Val x , Val y -> range x y
    | Val x , Pos -> SupTo x
    | Neg , Val y -> InfTo y
	
let e_add a b =
  match a , b with
    | Neg , Pos | Pos , Neg -> failwith "Range.e_add"
    | Neg , _ | _ , Neg -> Neg
    | Pos , _ | _ , Pos -> Pos
    | Val x , Val y -> Val (Z.add x y)
	
let e_mul a b =
  match a , b with
    | Neg , Pos | Pos , Neg -> Neg
    | Neg , Neg | Pos , Pos -> Pos
    | Val x , Pos | Pos , Val x -> if Z.positive x then Pos else Neg
    | Val x , Neg | Neg , Val x -> if Z.positive x then Neg else Pos
    | Val x , Val y -> Val (Z.mul x y)
	
(* ------------------------------------------------------------------------ *)
(* ---  Domain Interface                                                --- *)
(* ------------------------------------------------------------------------ *)
	
let e_inf = function
  | Top | InfTo _ -> Neg
  | SupTo x | Range(x,_) -> Val x
  | Bot -> Pos
      
let e_sup = function
  | Top | SupTo _ -> Pos
  | InfTo x | Range(_,x) -> Val x
  | Bot -> Neg
      
let inf_to = function
  | Top -> Top
  | Bot -> Bot
  | InfTo _ as d -> d
  | SupTo _ -> Top
  | Range(_,x) -> InfTo x
      
let sup_to = function
  | Top -> Top
  | Bot -> Bot
  | InfTo _ -> Top
  | SupTo _ as d -> d
  | Range(x,_) -> SupTo x
      
let opp = function
  | Top -> Top
  | Bot -> Bot
  | InfTo x -> SupTo (Z.opp x)
  | SupTo x -> InfTo (Z.opp x)
  | Range(x,y) -> Range(Z.opp y,Z.opp x) (* never empty *)
      
let cup a b = 
  match a , b with
    | Top,_ | _,Top -> Top
    | Bot,c | c,Bot -> c
    | SupTo _ , InfTo _ | InfTo _ , SupTo _ -> Top
    | _ ->
	e_range 
	  (e_min (e_inf a) (e_inf b))
	  (e_max (e_sup a) (e_sup b))
	  
let cap a b = 
  match a , b with
    | Top,c | c,Top -> c
    | Bot,_ | _,Bot -> Bot
    | _ ->
	e_range 
	  (e_max (e_inf a) (e_inf b))
	  (e_min (e_sup a) (e_sup b))
	  
let wide a b =
  match a , b with
    | Top,_ | _,Top -> Top
    | Bot,c | c,Bot -> c
    | _ ->
	e_range
	  (w_min (e_inf a) (e_inf b))
	  (w_max (e_sup a) (e_sup b))
	  
let add a b =
  match a , b with
    | Top,_ | _,Top -> Top
    | Bot,c | c,Bot -> c
    | _ ->
	e_range
	  (e_add (e_inf a) (e_inf b))
	  (e_add (e_sup a) (e_sup b))
	  
let times k = function
  | Top -> Top
  | Bot -> Bot
  | InfTo x -> 
      let y = Z.mul k x in
      if Z.positive k then InfTo y else SupTo y
  | SupTo x -> 
      let y = Z.mul k x in
      if Z.positive k then SupTo y else InfTo y
  | Range(a,b) ->
      let a' = Z.mul k a in
      let b' = Z.mul k b in
      (* never empty *)
      if Z.positive k then Range(a',b') else Range(b',a')
	
let mul a b =
  let a1 = e_inf a in
  let a2 = e_sup a in
  let b1 = e_inf b in
  let b2 = e_sup b in
  let rec select_range x y = function
    | [] -> e_range x y
    | e::es -> select_range (e_min x e) (e_max y e) es
  in select_range Pos Neg [
    e_mul a1 b1 ;
    e_mul a1 b2 ;
    e_mul a2 b1 ;
    e_mul a2 b2 ;
  ]
	
let subset a b =
  (e_cmp (e_inf a) (e_inf b) >= 0) &&
    (e_cmp (e_sup a) (e_sup b) <= 0)

open Logic

let are_leq a b =
  match a , b with
    | ( Range(_,x) | InfTo x ) , ( Range(y,_) | SupTo y ) when Z.leq x y -> Yes
    | ( Range(x,_) | SupTo x ) , ( Range(_,y) | InfTo y ) when Z.lt y x -> No
    | _ -> Maybe

let are_lt a b =
  match a , b with
    | ( Range(_,x) | InfTo x ) , ( Range(y,_) | SupTo y ) when Z.lt x y -> Yes
    | ( Range(x,_) | SupTo x ) , ( Range(_,y) | InfTo y ) when Z.leq y x -> No
    | _ -> Maybe
  

let pretty fmt = function
  | Top -> Format.fprintf fmt "[..]"
  | Bot -> Format.fprintf fmt "[]"
  | InfTo x -> Format.fprintf fmt "[..%a]" Z.pretty x
  | SupTo x -> Format.fprintf fmt "[%a..]" Z.pretty x  
  | Range(x,y) -> Format.fprintf fmt "[%a..%a]" Z.pretty x Z.pretty y
