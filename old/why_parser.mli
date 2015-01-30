(* -------------------------------------------------------------------------- *)
(** Parser for Terms                                                          *)
(* -------------------------------------------------------------------------- *)

open Syntax
open Lexer

val keymap : keymap
val parse_type : input -> t
val parse_expr : input -> e
val parse_declaration : input -> d
