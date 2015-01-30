(* -------------------------------------------------------------------------- *)
(* --- Proof Script Actions                                               --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Logic
open Syntax
open Lexer
open Ground
open Grammar

let keywords = [
  "quit" ; "include" ;
  "print" ; "type" ; "let" ;
  "proof" ; "qed" ; "end" ;
  "intro" ; "simplify" ;
]

let rec parse_list0 sep pp input =
  if is_key input sep then pp input :: parse_list0 sep pp input else []

let parse_idents input =
  match token input with
    | IDENT _ ->
	let x = skip_ident input in
	x :: parse_list0 "," skip_ident input
    | _ -> []

let parse_action input =
  match token input with
    | KEYWORD "proof" -> skip input ; A_proof
    | KEYWORD "qed" -> skip input ; A_qed
    | KEYWORD "end" -> skip input ; A_end
    | KEYWORD "intro" -> skip input ; A_intro (parse_idents input)
    | _ -> A_none
