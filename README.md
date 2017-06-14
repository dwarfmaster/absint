Introduction
============

This program is a abstract interpreter for a small subset of c. It supports
conditions, functions, arbitrary gotos, while and for loops, and int and bool
types (please be aware of bug #1). Also, while functions are supported, they
cannot be recursive : those will cause the interpret not to end. Furthermore,
by lack of time, there is no typecheck, thus the program entered should be
semantically correct. If they aren't, the program may not fail but produce
inconsistent output.

Supported abstractions
======================

Three abstractions are supported : constants, segments and finite sets (called
concrete by abuse of language). In all of these, integer are considered to
be in Z : it doesn't take into account modular arithmetic.

Finite sets store finite sets of values, and approximate any infinite set by
Z. That makes it really precise for specific computations, but as soon as
widening happens, all precision is lost : since it cannot represent half of
Z, guard over comparison brings no precision.

Constant domain is the same, except that the set are supposed to be of cardinal
1 or 0.

Segment bound the values taken by variables by a lower an upper bound, possibly
infinite. This is by far the most powerful abstraction when handling guards,
since it manages comparison very easily.

Building and using
==================

It can be built with the command `cabal build` if you have cabal. If it fails,
there is a nix file under which it works, ie the command
`nix-shell --command "cabal build"` should work. The produced binary is
under `dist/build/absint/absint`.

To use, pipe the C program you want analysed into the program. It will create
four files : `graph.pdf`, a drawing of the graph (/!\ it needs graphviz to be
installed and accessible under `$PATH` for this to work), `out.concrete`,
`out.constant` and `out.segment`. The `out.*` file starts with a binding from
variables ids to their name and position in the program, followed by the abstract
environment for each node. If a variable is not present in an environment, it
means its interpretation is bottom. To know to which part of the program each
node correspond, refer to the graph.

Missing features
================

There is a support for asserts and rand in the iterator and abstract domain,
but no syntactic way to introduce in the source code. Please note that rand
can be simulated by a loop (forcing a widening) followed by two conditions, but
only with the segment abstraction.

