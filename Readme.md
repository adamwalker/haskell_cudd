# Haskell-CUDD

Haskell bindings to version 3.0.0 of the CUDD binary decision diagram library.

http://vlsi.colorado.edu/~fabio/CUDD/

This package provides two interfaces to the CUDD library:
* A purely functional one in `Cudd.Cudd` that automatically dereferences BDDs during garbage collection.
* An ST Monad based one in `Cudd.Imperative` that gives you precise control over the ordering of BDD operations and when BDDs are dereferenced. Use this one if you want your code to perform well.

Also, for a higher level interface in the style of the [ersatz](https://hackage.haskell.org/package/ersatz) SAT encoder, see https://github.com/jwaldmann/cudd-ersatz/.

# Installation

Either install CUDD 3.0.0 using your system's package manager or download and install CUDD from here: http://vlsi.colorado.edu/~fabio/.

Then:

`cabal install cudd`

Depending on where CUDD is installed on your system, you may need to provide --extra-lib-dirs or --extra-include-dirs:

`cabal install cudd --extra-lib-dirs=/usr/local/lib`

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
