(* -------------------------------------------------------------------------- *)
(* --- WHY Pretty Printer with sharing                                    --- *)
(* -------------------------------------------------------------------------- *)

open Format
open Output

(* -------------------------------------------------------------------------- *)
(* --- Pretty-print Types                                                 --- *)
(* -------------------------------------------------------------------------- *)

let rec pp_tau fmt = function
  | T_ALPHA a -> Format.fprintf fmt "'%s" a
  | T_PROP -> pp_print_string fmt "bool"
  | T_REAL -> pp_print_string fmt "real"
  | T_INT  -> pp_print_string fmt "int"
  | T_DATA([],"") -> pp_print_char fmt '_'
  | T_DATA([],a) -> pp_print_string fmt a
  | T_DATA([t],a) -> fprintf fmt "%a %s" pp_tau t a
  | T_DATA(t::ts,a) -> 
      fprintf fmt "@[<hov 2>(%a" pp_tau t ;
      List.iter (fun t -> fprintf fmt ",%a" pp_tau t) ts ;
      fprintf fmt ") %s@]" a

(* -------------------------------------------------------------------------- *)
(* --- Pretty-print Terms                                                 --- *)
(* -------------------------------------------------------------------------- *)

let pp_var fmt (x,k) = fprintf fmt "%s_%d" x k

let pp_binop fmt = function
  | ADD -> pp_print_char fmt '+'
  | SUB -> pp_print_char fmt '-'
  | MUL -> pp_print_char fmt '*'
  | DIV -> pp_print_char fmt '/'
  | MOD -> pp_print_char fmt '%'
  | AND -> pp_print_string fmt " and "
  | OR  -> pp_print_string fmt " or "
  | IMPLY -> pp_print_string fmt " -> "
  | EQUIV -> pp_print_string fmt " <-> "
  | LT  -> pp_print_char fmt '<'
  | EQ  -> pp_print_char fmt '='
  | LEQ -> pp_print_string fmt "<="
  | NEQ -> pp_print_string fmt "<>"

let pp_for_binop = function
  | IMPLY -> Some "imply_bool"
  | AND -> Some "and_bool"
  | OR -> Some "or_bool"
  | LT -> Some "lt_int_bool"
  | LEQ -> Some "leq_int_bool"
  | _ -> None

let rec pp_as_term fmt = function
  | E_PRM k -> fprintf fmt "_%d" k
  | E_VAR x -> pp_var fmt x
  | E_INT z -> pp_print_string fmt (Z.to_string z)
  | E_REAL r -> pp_print_string fmt (R.to_string r)
  | E_TRUE -> pp_print_string fmt "true"
  | E_FALSE -> pp_print_string fmt "false"
  | E_BIN(a,op,b) -> 
      begin
	match pp_for_binop op with
	  | None ->
	      fprintf fmt "(%a%a%a)" pp_as_term a pp_binop op pp_as_term b
	  | Some f -> 
	      fprintf fmt "%s(%a,%a)" f pp_as_term a pp_as_term b
      end
  | E_UNA(OPP,a) -> fprintf fmt "(-%a)" pp_as_term a
  | E_UNA(NOT,a) -> fprintf fmt "not_bool(%a)" pp_as_term a
  | E_FUN(fct,[]) -> pp_print_string fmt fct
  | E_FUN(fct,e::es) -> 
      fprintf fmt "%s(%a" fct pp_as_term e ;
      List.iter (fun e -> fprintf fmt ",%a" pp_as_term e) es ;
      fprintf fmt ")"
  | E_LAMBDA(x,t,a) -> 
      fprintf fmt "@[<hov 0>fun %a:%a ->@]@ %a" pp_var x pp_tau t pp_as_term a
  | E_FORALL _ | E_EXISTS _ ->
      failwith "Quantified property in term position"
  | E_IF(e,a,b) -> 
      fprintf fmt "@[<hov 0>if %a@ then %a@ else %a@]" pp_as_term e pp_as_term a pp_as_term b
  | E_LET(x,a,b) -> 
      fprintf fmt "@[<hov 0>let %a=%a@ in@]@ %a" pp_var x pp_as_term a pp_as_term b

let rec pp_as_prop fmt = function
  | E_PRM k -> fprintf fmt "_%d" k
  | E_BIN(a,((AND|OR|IMPLY|EQUIV) as op),b) -> 
      fprintf fmt "(%a%a%a)" pp_as_prop a pp_binop op pp_as_prop b
  | E_VAR _ as e -> fprintf fmt "(%a=true)" pp_as_term e
  | E_TRUE -> pp_print_string fmt "true"
  | E_FALSE -> pp_print_string fmt "false"
  | (E_BIN _ | E_INT _ | E_REAL _ | E_UNA(OPP,_) ) as e -> 
      Log.failure "Non-property term: %a" pp_as_term e
  | E_UNA(NOT,a) -> fprintf fmt "(not %a)" pp_as_prop a
  | E_FUN(fct,[]) -> pp_print_string fmt fct
  | E_FUN(fct,e::es) -> 
      fprintf fmt "%s(%a" fct pp_as_term e ;
      List.iter (fun e -> fprintf fmt ",@,%a" pp_as_term e) es ;
      fprintf fmt ")"
  | E_LAMBDA(x,t,a) -> 
      fprintf fmt "@[<hov 0>fun %a:%a ->@]@ %a" pp_var x pp_tau t pp_as_prop a
  | E_FORALL(x,t,a) -> 
      fprintf fmt "@[<hov 0>forall %a:%a.@]@,%a" pp_var x pp_tau t pp_as_prop a
  | E_EXISTS(x,t,a) -> 
      fprintf fmt "@[<hov 0>exists %a:%a.@]@,%a" pp_var x pp_tau t pp_as_prop a
  | E_IF(e,a,b) -> 
      fprintf fmt "@[<hov 0>if %a@ then %a@ else %a@]" pp_as_term e pp_as_prop a pp_as_prop b
  | E_LET(x,a,b) -> 
      fprintf fmt "@[<hov 0>let %a=%a@ in@]@ %a" pp_var x pp_as_term a pp_as_prop b

let pp_repr f fmt = function
  | E_PRM k -> fprintf fmt "_%d" k
  | E_VAR x -> pp_var fmt x
  | E_INT z -> pp_print_string fmt (Z.to_string z)
  | E_REAL r -> pp_print_string fmt (R.to_string r)
  | E_TRUE -> pp_print_string fmt "true"
  | E_FALSE -> pp_print_string fmt "false"
  | E_BIN(a,op,b) -> fprintf fmt "(%a%a%a)" f a pp_binop op f b
  | E_UNA(OPP,a) -> fprintf fmt "(-%a)" f a
  | E_UNA(NOT,a) -> fprintf fmt "(not %a)" f a
  | E_FUN(fct,[]) -> pp_print_string fmt fct
  | E_FUN(fct,e::es) -> 
      fprintf fmt "%s(%a" fct f e ;
      List.iter (fun e -> fprintf fmt ",@,%a" f e) es ;
      fprintf fmt ")"
  | E_LAMBDA(x,t,a) -> 
      fprintf fmt "@[<hov 0>fun %a:%a ->@]@ %a" pp_var x pp_tau t f a
  | E_FORALL(x,t,a) -> 
      fprintf fmt "@[<hov 0>forall %a:%a.@]@,%a" pp_var x pp_tau t f a
  | E_EXISTS(x,t,a) -> 
      fprintf fmt "@[<hov 0>exists %a:%a.@]@,%a" pp_var x pp_tau t f a
  | E_IF(e,a,b) -> 
      fprintf fmt "@[<hov 0>if %a@ then %a@ else %a@]" f e f a f b
  | E_LET(x,a,b) -> 
      fprintf fmt "@[<hov 0>let %a=%a@ in@]@ %a" pp_var x f a f b

let rec pp_expr fmt e = pp_repr pp_expr fmt e
let pp_term fmt e = pp_as_term fmt e
let pp_prop fmt e = pp_as_prop fmt e
