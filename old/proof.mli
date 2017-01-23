(* -------------------------------------------------------------------------- *)
(** Proof Representation and Checking                                         *)
(* -------------------------------------------------------------------------- *)

open Logic

module Make(T : Term) :
sig

  type context

  val empty : context
  val assume : context -> T.term -> context option
    (** [None] means that context is inconsistent : it implies False *)
  val assume_all : context -> T.term list -> context option
    (** [None] means that context is inconsistent : it implies False *)

  val check_true  : context -> T.term -> maybe
  val check_false : context -> T.term -> maybe

  type proof =

    | Compute
	(** Recomputing goal leads to tautology *)

    | Assumptions of proof
	(** [Assumptions P]:
	    - check goal is [H1..Hn -> g],
	    - assume [H1..Hn],
            - then prove [g] with [P]. *)

    | Cut of ( T.term * proof ) list * proof
	(** [Cut( [H1,P1 ;... ; Hn,Pn] , Pgoal )]:
	     - prove [Hi] with [Pi], then assume [H1...Hn]
	     - then prove goal with [Pgoal]. *)

    | Cases of ( T.term * proof ) list * proof
	(** [Cases( [H1,P1 ;... ; Hn,Pn] , Pfull )]:
	     - prove goal with [Pi] under hypothesis [Hi],
	     - then prove disjunction of [Hi] with [Pfull]. *)

    | Rewrite of T.term * T.term * proof * proof
	(** [Rewrite(a,b,Peq,Pgoal)]:
	    - prove [a=b] with [Peq], 
            - then replace [a] by [b] in goal,
	    - finish the proof with [Pgoal]. *)

    | Apply of T.term * proof * link list * proof
	(** [Apply(Thm,Pthm,L1..Ln,Pgoal)]:
	    - prove [Thm] with [Pthm],
	    - check [Thm] is [Q1,..,Qn.P],
	    - when [Qi=forall x], check [Li=Lterm t],
	    - when [Qi=exists x], check [Li=Lvar y] and [y] is fresh,
	    - assume [P[xi:=Li]],
	    - finally prove goal with [Pgoal]. *)

    | Intro of link list * proof
	(** [Intro(L1..Ln,Pgoal)]:
	    - check goal is [Q1,..,Qn.P],
	    - when [Qi=forall xi], check [Li=Lvar y] and [y] is fresh,
	    - when [Qi=exists xi], check [Li=Lterm t],
	    - finally prove [P[xi:=Li]] witrh [Pgoal]. *)

    | Induction of T.term * proof * proof * T.var * proof
	(** [Induction(a,Ppos,Pinit,n,Pind)]:
	    - prove [a>=0] with [Ppos],
	    - prove goal with [Pinit] with hypothesis [a=0],
	    - check [n] is fresh, let [xs] is free variables of [a] and [goal]: 
	    - prove goal with [Pind] with hypotheses:
	    -- [Hn: forall xs. 0 <= a <= n -> goal]
	    -- [Ha: a = n+1] *)
	    
  and link = Lvar of T.var | Lterm of T.term

  exception Incomplete   (** Proof does not terminates on tautology *)
  exception Inconsistent (** Proof leads to contradiction *)

  val check : context -> T.term -> proof -> unit

end
