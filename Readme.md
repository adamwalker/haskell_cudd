# Haskell-CUDD

Haskell bindings to the CUDD binary decision diagram library.

http://vlsi.colorado.edu/~fabio/CUDD/

# Installation

Either install CUDD using your system's package manager or download and build CUDD from here: https://github.com/adamwalker/cudd. This is a mirror of the CUDD source that has been modified to build shared object files.

If you chose the latter option you need to tell cabal where to find cudd:

`cabal install cudd --extra-include-dirs=/path/to/cudd/src/include --extra-lib-dirs=/path/to/cudd/src/libso`

and you need to tell your program where to find the shared libraries:

`LD_LIBRARY_PATH=/path/to/cudd/src/libso ghci`
