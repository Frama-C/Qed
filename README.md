# First Order Logic with Theories

**Qed** is an OCaml library for representing efficiently terms and propositions of first order logic.
The library comes with built-in theories for integer and real arithmetics, arrays, records, uninterpreted functions
with algebraic properties (associativity and such).

The library provides strong normalization to cannonical represents and maximal in-memory sharing _via_ smart constructors.
Pretty-printing and export to other language is provided _via_ a flexible pretty-printing engine.
