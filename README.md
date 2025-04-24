COOSy (Curry Object Observation System)
=======================================

# Overview

COOSy is an implementation of a lightweight approach
for debugging functional logic programs written in Curry by observations.
It consists of a library plus a viewing tool.
Programmers can annotate their program with observation functions for data
structures and functions which may both contain or be applied to logical
variables.
The parts of observed objects that are evaluated during the execution are
recorded in a trace file and can be viewed by means of a pretty printer
integrated in a graphical user interface.
The tool covers all aspects of modern functional logic
multiparadigm languages such as lazy evaluation, higher order functions,
non-deterministic search, logic variables, concurrency and constraints.

# Implementation

The implementation of COOSy is freely available and an interface to COOSy
is integrated in the distribution of the Curry programming environment
[PAKCS](https://www.curry-lang.org/pakcs).
To use COOSy in PAKCS, the implementation must be installed locally,
which can be done by installing this package with the command

    cypm install coosy

# Documentation

Details about COOSy and its implementation can be found in
this paper:

B. Brassel, O. Chitil, M. Hanus, F. Huch:
[Observing Functional Logic Computations](https://doi.org/10.1007/978-3-540-24836-1_14),
Proc. of the Sixth International Symposium on
Practical Aspects of Declarative Languages (PADL'04),
Springer LNCS 3057, pp. 193-208, 2004

There are also
[slides about COOSy](https://www.michaelhanus.de/slides/PADL04.pdf)
which have been presented at PADL 2004.

# Important note

In contrast to the version described in the original paper, the current
version also considers the fact that free variables can only occur
in expressions with a `Data` context (see the paper
[Adding Data to Curry](https://doi.org/10.1007/978-3-030-46714-2_15)
for a detailed explanation). Therefore, the current observation tool
distinguishes the observation of expressions with or without a
`Data` context: the operation `Observe.observe` can observe
expressions containing free variables which are instantiated
during the evaluation (which is the main innovation of COOSy),
whereas the operation `Observe.observeG` can also observe
expressions without a `Data` context but requires that they are always
instantiated (otherwise, the observation might suspend).
Since functional values have no `Data` context, one has to distinguish
how to observe the arguments. For instance, observers with observer type

    (oList oInt ~> oList oInt)

can observe functions which are possibly invoked with free variables and
might return a free variable, whereas

    (oList oInt ~~> oList oInt ~~> oInt)

observe functions which are possibly invoked with free variables but
return a non-free results and

    (oList oInt ~~~> oList oInt ~~~> oMaybe oInt)

observe functions which are called with non-free arguments and return
non-free results.

The `examples` directory contains various programs containing observers.

-------------------------------------------------------------------------
Developed by

* Bernd Brassel (CAU Kiel, Germany, bbr@informatik.uni-kiel.de)
* Olaf Chitil   (University of Kent, UK, O.Chitil@kent.ac.uk)
* Michael Hanus (CAU Kiel, Germany, mh@informatik.uni-kiel.de)
* Frank Huch    (CAU Kiel, Germany, fhu@informatik.uni-kiel.de)

Version of September 20, 2024

-------------------------------------------------------------------------
