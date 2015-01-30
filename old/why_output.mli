(* -------------------------------------------------------------------------- *)
(**    Pretty Printer for Why Output.                                         *)
(* -------------------------------------------------------------------------- *)

open Format
open Output

val pp_tau : formatter -> tau -> unit
val pp_term : formatter -> term -> unit (** terms only *)
val pp_prop : formatter -> term -> unit (** predicates only *)
val pp_expr : formatter -> term -> unit (** either terms or predicates *)
