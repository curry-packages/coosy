COOSy (Curry Object Observation System)
=======================================

This package contains the implementation of a tool to observe
the execution of programs written
in the declarative multi-paradigm language Curry

Details about COOSy and its implementation can be found in
this paper:

B. Brassel, O. Chitil, M. Hanus, F. Huch:
[Observing Functional Logic Computations](https://doi.org/10.1007/978-3-540-24836-1_14),
Proc. of the Sixth International Symposium on
Practical Aspects of Declarative Languages (PADL'04),
Springer LNCS 3057, pp. 193-208, 2004


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


Version of February 27, 2023

-------------------------------------------------------------------------
