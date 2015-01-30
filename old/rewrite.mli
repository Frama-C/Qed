(* -------------------------------------------------------------------------- *)
(** Naive rewriting engine.                                                   *)
(* -------------------------------------------------------------------------- *)

(** {1 Generic Strategy} *)

module Make (T : Logic.Term) :
sig

  type compute = T.term -> T.term array -> T.term

  val rewrite  : 
    (T.f,T.t) Logic.rewrite list -> compute -> 
    T.f -> T.term list -> T.term option

  (** Applies the given rule at the given position in the term.
      Returns the guard and the simplified term. If the position is incorrect
      or the rule does not apply, the guard is [true] and the term unchanged.
   *)
  val rewrite_rule_at_pos:
    (T.f,T.t) Logic.rewrite -> compute -> 
    T.term -> T.path -> (T.term * T.term)

end

(** {1 Rewriter for [Ground]} *)

open Ground

val compute : T.term -> T.term array -> T.term

val simplify : T.term -> T.term

val rewrite_rule_at_pos: (T.f,T.term) Logic.rewrite -> 
  T.term -> T.path -> (T.term * T.term)
