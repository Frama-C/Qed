# Qed

Qed is an [OCaml](http://ocaml.org) library implementing first-order logic with built-in theories.
It is developped by CEA-Tech in the Sofware Reliability and Security Lab, and shiped with the [Frama-C](http://frama-c.com) platform 
for proving ACSL contracts by Weakest-Precondition (WP).

The design of the library is described in the article
« [Qed: Computing What Remains to be Proved] (http://link.springer.com/chapter/10.1007/978-3-319-06200-6_17) »
published at NASA Formal Method, 2015.

The main features of the library are:
 - boolean theory
 - quantifiers
 - integer and real theories
 - array and record theories
 - uninterpreted functions, extended by associativy, commutativity, neutral and absorbants, injectivity, constructors and inversibility
 - extensible rewriters for interpreting functions
 - fully normalized terms _wrt_ theories
 - combinators for memoization, substitution, shared sub-terms
 - flexible pretty-printing
 - export engines to SMT solvers and proof assistants

# License

The Qed Library is distributed under the Gnu Lesser General Public License,
Version 2.1, a [copy](licenses/LGPLv2.1) of which is available in the repository.

Copyright CEA 2009
