(* -------------------------------------------------------------------------- *)
(** Term Simplifier                                                           *)
(* -------------------------------------------------------------------------- *)

open Logic

exception NoSolution

(** A set of equality relations. *)
class type ['a] eqs =
object
  method repr : 'a -> 'a
    (** [eqs#repr e] is the canonical represent of [e / eqs] *)
  method equal : 'a -> 'a -> maybe
    (** [eqs#equal a b] represents [a=b / eqs] *)
  method merge : 'a -> 'a -> unit
    (** [eqs#merge a b] asserts that [a] and [b] should be in the same class. *)
  method iter : ('a -> 'a -> unit) -> 'a -> unit
    (** [eqs#iter f x] iterates [f y x0] with [y] in [x / eqs] and [x0] 
	the canonical represent of [x]. *)
end

(** 
    A simplifier plugin. Maintain a set of properties.
    All methods should raise [NoSolution] if working properties are contradictory.
*)
class type ['a] simplifier =
object
  method copy : 'a simplifier 
    (** Duplicates the set of properties *)
  method compute : 'a eqs -> 'a -> 'a 
    (** Evaluate an expression w.r.t known properties *)
  method assume : 'a eqs -> 'a -> unit
    (** Build properties assuming the given formula is true. 
	The simplifier can be assumed to be normalized yet, 
	and to be renormalized after. *)
  method normalize : 'a eqs -> unit 
    (** Normalize the set of known properties w.r.t to known equalities *)
end

type 'a t = 'a simplifier

module Make(T : Term) : 
sig

  val simplify : T.term simplifier list -> T.term -> T.term
    (** Might raises [NoSolution] from a plugin at initialization
	stage.  Otherwize, [NoSolution] exceptions are likely to be
	catched during simplification, hence pruning any contradictory
	hypothesis. *)

end
