(** Proof Grammar *)

open Qed
open Syntax

(**
   {3 Proof}
   - [ proof  := "proof" script "qed" ]
   - [ script := action... strategy ]
   - [ subproof := "proof" script "end" | tactic ]

   {3 Actions}
   - [ action := "intro" id... ]
   - [ action := "have" prop ] (straightforward proof)
   - [ action := "have" prop subproof ]
   - [ action := "let" id (:type)? = exp ]

   {3 Strategy and Tactics}
   Tactics can be used as strategy.
   - [ strategy := ("case" H: script)+ ("otherwise": script)? ]
   - [ tactic := "straightforward" ]
   - [ tactic := "by" theorem ]

*)

type action =
  | A_none
  | A_proof
  | A_qed
  | A_end
  | A_intro of id list
