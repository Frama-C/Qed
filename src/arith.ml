(**************************************************************************)
(*                                                                        *)
(*  This file is part of Qed Library                                      *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Natural arithmetics. *)

module type Z =
sig

  type t

  val zero : t
  val one : t
  val minus_one : t

  val succ : t -> t
  val pred : t -> t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t

  val div : t -> t -> t (* acsl division  *)
  val rem : t -> t -> t (* acsl remainder *)
  val div_rem : t -> t -> t * t   (* acsl *)

  val equal : t -> t -> bool

  val leq : t -> t -> bool
  val lt  : t -> t -> bool

  val of_int : int -> t
  val of_string : string -> t
  val to_string : t -> string

  val hash : t -> int
  val compare : t -> t -> int

end