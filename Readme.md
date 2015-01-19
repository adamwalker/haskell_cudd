# Haskell-CUDD

Haskell bindings to version 2.5.0 of the CUDD binary decision diagram library.

http://vlsi.colorado.edu/~fabio/CUDD/

This package provides two interfaces to the CUDD library:
* A purely functional in one `Cudd.Cudd` that automatically dereferences BDDs during garbage collection
* An ST Monad based one in `Cudd.Imperative` that gives you precise control over the ordering of BDD operations and when BDDs are dereferenced. Use this one if you want your code to perform well.

# Installation

Either install CUDD using your system's package manager or download and build CUDD from here: https://github.com/adamwalker/cudd. This is a mirror of the CUDD source that has been modified to build shared object files.

If you chose the latter option you need to tell cabal where to find cudd:

`cabal install cudd --extra-include-dirs=/path/to/cudd/src/include --extra-lib-dirs=/path/to/cudd/src/libso`

and you need to tell your program where to find the shared libraries:

`LD_LIBRARY_PATH=/path/to/cudd/src/libso ghci`

# Usage

The purely functional interface:

```haskell
import Cudd.Cudd

main = do
    let manager = cuddInit
        v1      = ithVar manager 0
        v2      = ithVar manager 1
        conj    = bAnd manager v1 v2
        implies = lEq manager conj v1
    print implies

import Control.Monad.ST
import Cudd.Imperative
```

The ST Monad based interface:

```haskell
import Control.Monad.ST
import Cudd.Imperative

main = do
    res <- stToIO $ withManagerDefaults $ \manager -> do
        v1      <- ithVar manager 0
        v2      <- ithVar manager 1
        conj    <- bAnd manager v1 v2
        implies <- lEq manager conj v1
        deref manager conj
        return implies
    print res
```
