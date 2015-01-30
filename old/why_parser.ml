(* -------------------------------------------------------------------------- *)
(* --- PARSER                                                             --- *)
(* -------------------------------------------------------------------------- *)

open Syntax
open Lexer

let keymap = Lexer.keymap [
  "int" ; "bool" ; "real" ; "unit" ; "prop" ;
  "true" ; "false" ; "void" ; "and" ; "or" ; "not" ;
  "if" ; "then" ; "else" ;
  "let" ; "forall" ; "exists" ;
  "logic" ; "axiom" ; "predicate" ; "function" ; "type" ; "goal" ;
]

let parse_recursion = ref (fun _ -> assert false)
    
(* -------------------------------------------------------------------------- *)
(* --- Types                                                              --- *)
(* -------------------------------------------------------------------------- *)

let parse_simpletype input =
  match token input with
    | KEYWORD "int" -> skip input ; T_INT
    | KEYWORD "bool" -> skip input ; T_BOOL
    | KEYWORD "prop" -> skip input ; T_PROP
    | KEYWORD "real" -> skip input ; T_REAL
    | KEYWORD "unit" -> let u = skip_pos input,"unit" in T_SORT([],u)
    | QUOTED x -> let p = skip_pos input in T_ALPHA (p,x)
    | IDENT x -> let p = skip_pos input in T_SORT([],(p,x))
    | _ -> error input "missing type"

let rec parse_polymorphic t input =
  match token input with
    | IDENT x -> 
	let id = skip_pos input,x in
	parse_polymorphic (T_SORT([t],id)) input
    | _ -> t

let rec parse_type input =
  context input "type" ;
  match parse_typelist input with
    | Some ts ->
	let id = skip_ident input in
	parse_polymorphic (T_SORT(ts,id)) input
    | None ->
	let t = parse_simpletype input in
	parse_polymorphic t input

and parse_typelist input =
  parse_list ~left:"(" ~sep:"," ~right:")" parse_type input

(* -------------------------------------------------------------------------- *)
(* --- Atoms                                                              --- *)
(* -------------------------------------------------------------------------- *)

let parse_arguments input =
  match parse_list ~left:"(" ~sep:"," ~right:")" !parse_recursion input with
    | Some xs -> xs
    | None -> []

let parse_atom input =
  match token input with
    | INT x   -> E_INT(skip_pos input,x)
    | REAL x  -> E_REAL(skip_pos input,x)
    | KEYWORD "true" -> E_TRUE (skip_pos input)
    | KEYWORD "false" -> E_FALSE (skip_pos input)
    | IDENT x -> 
	let id = skip_pos input,x in 
	let xs = parse_arguments input in
	E_FUN(id,Ast.fresh(),xs)
    | KEYWORD "(" ->
	skip input ;
	let e = !parse_recursion input in
	skip_key input ")" ; e
    | _ -> error input "term expected"

(* -------------------------------------------------------------------------- *)
(* --- Arrays                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_access_update main input =
  let p = position input in
  if is_key input "[" then
    let e = !parse_recursion input in
    if is_key input "<-" then
      let v = !parse_recursion input in
      let id = p,"update" in 
      let t = E_FUN(id,Ast.fresh(),[main;e;v])(* main[e<-v] *) in
      skip_key input "]" ;
      parse_access_update t input
    else
      let id = p,"access" in 
      let t = E_FUN(id,Ast.fresh(),[main;e])(* main[e] *) in
      skip_key input "]" ;
      parse_access_update t input
  else
    main
  
let parse_array input = parse_access_update (parse_atom input) input
    
    

(* -------------------------------------------------------------------------- *)
(* --- Factors                                                            --- *)
(* -------------------------------------------------------------------------- *)
    
let parse_factor input =
  context input "factor" ;
  match token input with
    | KEYWORD "-" ->
	let p = skip_pos input in
	let a = parse_array input in
	E_UNA(p,OPP,a)
    | _ -> parse_array input

(* -------------------------------------------------------------------------- *)
(* --- Divisions                                                          --- *)
(* -------------------------------------------------------------------------- *)

let parse_division input =
  let a = parse_factor input in
  match token input with
    | KEYWORD "%" ->
	let p = skip_pos input in
	let b = parse_factor input in
	E_BIN(a,p,MOD,b)
    | KEYWORD "/" ->
	let p = skip_pos input in
	let b = parse_factor input in
	E_BIN(a,p,DIV,b)
    | _ -> a

(* -------------------------------------------------------------------------- *)
(* --- Multiplications                                                    --- *)
(* -------------------------------------------------------------------------- *)

let parse_multiplicative input = 
  let rec pp_mult x input =
    match token input with
      | KEYWORD "*" ->
	  let p = skip_pos input in
	  let y = parse_division input in
	  let z = E_BIN(x,p,MUL,y) in
	  pp_mult z input
      | _ -> x
  in  
  context input "multiplicative" ;
  pp_mult (parse_division input) input

(* -------------------------------------------------------------------------- *)
(* --- Additions                                                          --- *)
(* -------------------------------------------------------------------------- *)

let parse_additive input =
  let rec pp_add x input =
    match token input with
      | KEYWORD "+" ->
	  let p = skip_pos input in
	  let y = parse_multiplicative input in
	  let z = E_BIN(x,p,ADD,y) in
	  pp_add z input
      | KEYWORD "-" ->
	  let p = skip_pos input in
	  let y = parse_multiplicative input in
	  let z = E_BIN(x,p,SUB,y) in
	  pp_add z input
      | _ -> x
  in  
  context input "additive" ;
  pp_add (parse_multiplicative input) input

(* -------------------------------------------------------------------------- *)
(* --- Relations                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_ascending x input =
  match token input with
    | KEYWORD "<" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,LT,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,AND,w)
    | KEYWORD "<=" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,LEQ,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,AND,w)
    | _ -> x
	
let rec parse_descending x input =
  match token input with
    | KEYWORD ">" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,GT,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,ADD,w)
    | KEYWORD ">=" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,GEQ,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,AND,w)
    | _ -> x

let parse_relation input =
  context input "relation" ;
  let x = parse_additive input in
  match token input with
    | KEYWORD ("<" | "<=") -> (* no skip *) parse_ascending x input
    | KEYWORD (">" | ">=") -> (* no skip *) parse_descending x input
    | KEYWORD "=" ->
	let p = skip_pos input in
	let y = parse_additive input in
	E_BIN(x,p,EQ,y)
    | KEYWORD "<>" ->
	let p = skip_pos input in
	let y = parse_additive input in
	E_BIN(x,p,NEQ,y)
    | _ -> x

(* -------------------------------------------------------------------------- *)
(* --- Logical                                                            --- *)
(* -------------------------------------------------------------------------- *)

let parse_clause input =
  match token input with
    | KEYWORD "not" ->
	let p = skip_pos input in
	let e = parse_array input in
	E_UNA(p,NOT,e)
    | _ ->
	parse_relation input

let parse_logical input =
  let rec pp_log x input =
    match token input with
      | KEYWORD "and" ->
	  let p = skip_pos input in
	  let y = parse_clause input in
	  let z = E_BIN(x,p,AND,y) in
	  pp_log z input
      | KEYWORD "or" ->
	  let p = skip_pos input in
	  let y = parse_clause input in
	  let z = E_BIN(x,p,OR,y) in
	  pp_log z input
      | _ -> x
  in context input "logical" ;
  let x = parse_clause input in
  pp_log x input

let rec parse_idents input =
  let x = skip_ident input in
  if is_key input "," then x :: parse_idents input else [x]

let rec parse_bindings binder input =
  context input "bindings" ;
  let xs = parse_idents input in
  let t = if is_key input ":" then Some(parse_type input) else None in
  let triggers = 
    match Lexer.parse_list ~left:"[" ~sep:"," ~right:"]" !parse_recursion input with
      | None -> []
      | Some tgs -> tgs
  in
  let p =
    match token input with
      | KEYWORD "." -> 
	  skip input ;
	  !parse_recursion input
      | KEYWORD "," ->
	  skip input ;
	  parse_bindings binder input
      | _ ->
	  error input "expected '.' or ','"
  in
  List.fold_right (fun x p -> binder x t triggers p) xs p

(* -------------------------------------------------------------------------- *)
(* --- Left-to-right (deductive and bindings)                             --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_deductive input =
  context input "deductive" ;
  match token input with
    | KEYWORD "let" ->
	skip input ;
	let id = skip_ident input in
	skip_key input "=" ;
	let a = parse_deductive input in
	skip_key input "in" ;
	let b = parse_deductive input in
	E_LET(id,Ast.fresh(),None,a,b)
    | KEYWORD "forall" ->
	skip input ;
	parse_bindings (fun x t tgs p -> E_FORALL(x,Ast.fresh(),t,tgs,p)) input
    | KEYWORD "exists" ->
	skip input ;
	parse_bindings (fun x t tgs p -> E_EXISTS(x,Ast.fresh(),t,tgs,p)) input
    | KEYWORD "if" ->
	skip input ;
	let e = parse_logical input in
	skip_key input "then" ;
	let a = parse_logical input in
	skip_key input "else" ;
	let b = parse_logical input in
	E_IF(e,Ast.fresh(),a,b)
    | _ -> 
	let x = parse_logical input in
	match token input with
	  | KEYWORD "->" ->
	      let p = skip_pos input in
	      let y = parse_deductive input in
	      E_BIN(x,p,IMPLY,y)
	  | KEYWORD "<->" ->
	      let p = skip_pos input in
	      let y = parse_logical input in
	      E_BIN(x,p,EQUIV,y)
	  | _ -> x


(* -------------------------------------------------------------------------- *)
(* --- Root Expressions                                                   --- *)
(* -------------------------------------------------------------------------- *)

let parse_expr = parse_deductive
let () = parse_recursion := parse_deductive

(* -------------------------------------------------------------------------- *)
(* --- Declarations                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_quoted xs input =
  match token input with
    | QUOTED a -> let p = skip_pos input in parse_quoted ((p,a)::xs) input
    | IDENT a -> let p = skip_pos input in List.rev xs , (p,a)
    | _ -> error input "expected type name or polymorphic type variable"
let parse_typedef input = parse_quoted [] input

let rec parse_signature ts input =
  let t = parse_type input in
  match token input with
    | KEYWORD "->" ->
	skip input ;
	let tr = parse_type input in
	List.rev (t::ts) , tr
    | KEYWORD "," ->
	skip input ;
	parse_signature (t::ts) input
    | _ when ts=[] -> [],t
    | _ -> error input "Missing ',' or '->' in signature"

let parse_arg input =
  let x = skip_ident input in
  skip_key input ":" ;
  let t = parse_type input in
  x , Ast.fresh () , Some t

let parse_args input =
  match parse_list ~left:"(" ~sep:"," ~right:")" parse_arg input with
    | None -> []
    | Some xs -> xs

let parse_declaration input =
  Ast.reset () ;
  match token input with
    | KEYWORD "type" ->
	skip input ; 
	let xs,x = parse_typedef input in 
	D_TYPE(xs,x)
    | KEYWORD "logic" ->
	skip input ;
	let f = skip_ident input in
	skip_key input ":" ;
	let ts,tr = parse_signature [] input in
	D_DEF(f,Logic.Function,ts,tr)
    | KEYWORD "predicate" ->
	skip input ;
	let f = skip_ident input in
	let args = parse_args input in
	skip_key input "=" ;
	let value = parse_expr input in
	D_LET(f,args,Some T_PROP,value)
    | KEYWORD "function" ->
	skip input ;
	let f = skip_ident input in
	let args = parse_args input in
	let tr = parse_option ":" parse_type input in
	skip_key input "=" ;
	let value = parse_expr input in
	D_LET(f,args,tr,value)
    | KEYWORD "axiom" ->
	skip input ;
	let a = skip_ident input in
	skip_key input ":" ;
	let p = parse_expr input in
	D_AXIOM(a,p)
    | KEYWORD "goal" ->
	skip input ;
	let a = skip_ident input in
	skip_key input ":" ;
	let p = parse_expr input in
	D_GOAL(a,p)
   | _ -> 
	error input "declaration expected."

(* -------------------------------------------------------------------------- *)
