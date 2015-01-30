open Logic

(** Equivalence relation modulo equalities *)

module Make(T : Term) :
sig

  (** Complexity of operations refer to [d], the number of different sub-terms
      in their arguments. Hence, [d] is the size of the most compact dag that
      represent arguments. *)

  open T

  exception Contradiction

  type eqs 
    (** Set of equalities *)

  val class_of : eqs -> term -> Tset.t
    (** Returns the set of equals elements. Complexity is amortized [O(1)]. *)

  val repr_of : eqs -> term -> term
    (** Returns the represent. Complexity is [O(1)]. *)

  val iter : ( term -> Tset.t -> unit ) -> eqs -> unit
    (** Only over non-singleton classes of equivalence. *)

  val identity : eqs
    (** The class of [t] is the singleton [t]. *)

  val compute : eqs -> term -> term
    (** Recursively replace sub-terms by their representent. 
	@raise Contradiction if computation introduce inequalities. *)

  val are_equal : eqs -> term -> term -> maybe

  val merge : eqs -> term -> term -> eqs
    (** Add a new equality. Recompute the fixpoint of all cascading merges. 
	@raise Contradiction if the computations introduce non-equal terms. *)

  val assume : eqs -> term -> eqs
    (** Add a new hypothesis [h].
	If [h] is a [a=b], merges [a] and [b]. Otherwize,
	merges [h] with [true] and [not h] with [false]. 
	@raise Contradiction if the computations introduce non-equal terms. *)

end
