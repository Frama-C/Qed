(** {Abstract domain with intervals} *)

type d
val top : d
val bot : d
  
val singleton : Z.t -> d
val range : Z.t -> Z.t -> d

val cup : d -> d -> d
val cap : d -> d -> d
val subset : d -> d -> bool

val is_top : d -> bool
val is_empty : d -> bool
val is_singleton : d -> Z.t option

val are_lt : d -> d -> Logic.maybe
val are_leq : d -> d -> Logic.maybe
  
val opp : d -> d
val add : d -> d -> d
val mul : d -> d -> d
val times : Z.t -> d -> d
  
val inf_to : d -> d (* returns [[..max d]] *)
val sup_to : d -> d (* returns [[min d..]] *)

val pretty : Format.formatter -> d -> unit
