(** Prover Assistant Parser *)

open Qed
open Syntax
open Lexer
open Grammar

val keywords : string list
val parse_action : input -> action
