import Cudd.Cudd

main = do
    let manager = cuddInit
        v1      = ithVar manager 0
        v2      = ithVar manager 1
        conj    = bAnd manager v1 v2
        implies = lEq manager conj v1
    print implies

