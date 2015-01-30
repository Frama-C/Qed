open Qed
open Logic
open Action
open Ground
open Grammar

type state

val goal : env -> term -> state
val apply : state -> action -> state
val pretty : Format.formatter -> state -> unit
