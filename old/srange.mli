open Logic

(** Abstract domain for set of integers *)
module type Domain =
sig

  type d
  val top : d
  val cap : d -> d -> d
  val subset : d -> d -> bool
  val is_top : d -> bool
  val is_empty : d -> bool
  val is_singleton : d -> Z.t option
  val singleton : Z.t -> d

  val opp : d -> d
  val add : d -> d -> d
  val mul : d -> d -> d
  val times : Z.t -> d -> d

  val inf_to : d -> d 
    (** the set [[..sup d]] *)

  val sup_to : d -> d 
    (** the set [[inf d..]] *)

  val are_leq : d -> d -> Logic.maybe 
    (** property [x <= y] for all [x in d1] and [y in d2] *)

  val are_lt  : d -> d -> Logic.maybe 
    (** property [x < y] for all [x in d1] and [y in d2] *)

  val pretty : Format.formatter -> d -> unit

end

(** A Theory for arithmetic comparisons *)
module Make(T : Term)(D : Domain) :
sig

end

module Interval : Domain (** {!Range} module *)
