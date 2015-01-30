(* ------------------------------------------------------------------------ *)
(* ---  Conical Spaces                                                  --- *)
(* ------------------------------------------------------------------------ *)

type 'a merge = 'a -> 'a -> 'a option

let rec add ~order x = function
  | [] -> [x]
  | y::ys ->
      match order x y with
	| Some z -> add ~order z ys
	| None -> y::add ~order x ys
	    
let rec addlist ~order xs = function
  | [] -> xs
  | y::ys -> addlist ~order (add ~order y xs) ys

let base = addlist []
  
let rec shorter xs ys =
  match xs,ys with
    | [],_ -> true
    | _,[] -> false
    | _::xs , _::ys -> shorter xs ys
	
let merge ~order xs ys =
  match xs , ys with
    | [] , zs | zs , [] -> zs
    | [x] , zs | zs , [x] -> add ~order x zs
    | _ ->
	if shorter xs ys then addlist ~order ys xs else addlist ~order xs ys

let rec map ~order f = function
  | [] -> []
  | y::ys -> add ~order (f y) (map ~order f ys)
