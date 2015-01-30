open Logic

(** Simplifier for comparisons. *)

module Make(T : Term) :
sig

  val simplifier : unit -> T.term Simplifier.t

end
