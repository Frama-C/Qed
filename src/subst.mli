(* -------------------------------------------------------------------------- *)
(** Generalized Substitution (Congruence Closure) *)
(* -------------------------------------------------------------------------- *)

module Make(T : Logic.Term) :
sig

  type sigma (** A generalized substitution *)

  val create : unit -> sigma (** Create an empty substitution *)
  val find : sigma -> T.term -> T.term (** Lookup normal formal *)
  val merge : sigma -> T.term -> T.term -> unit (** Make two terms equal *)
  val copy : sigma -> sigma (** Copy the substitution. *)

end
