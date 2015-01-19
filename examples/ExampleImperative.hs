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
