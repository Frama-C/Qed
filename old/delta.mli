(* ------------------------------------------------------------------------ *)
(* ---  Proof Environments                                              --- *)
(* ------------------------------------------------------------------------ *)

type 'a comparison =
  | NotEqual
  | Equal of 'a
  | Compare of 'a * 'a

module type S =
sig

  module T : Logic.Term

  type env (** A simplification Environment *)
  exception Contradiction (** The current environment became inconsistent *)

  class type simplifier =
  object
    method copy     : simplifier
    method assumed  : env -> T.Tset.t -> unit
    method reduced : env -> T.Tset.t -> unit
    method simplify : env -> T.term -> T.term
  end

  val create   : T.pool -> env
  val copy     : env -> env

  val class_of : env -> T.term -> T.Tset.t
  val repr_of  : env -> T.term -> T.term
  val compare  : env -> T.term -> T.term -> T.term comparison

  val merge    : env -> T.term -> T.term -> unit
  val assume   : env -> T.term -> unit
  val compute  : env -> T.term -> T.term array -> T.term
  val simplify : env -> T.term -> T.term
  val register : env -> simplifier -> unit

end

module Make (T : Logic.Term) : S with module T = T
