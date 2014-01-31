module CuddCommon where

data SatBit = Zero | One | DontCare deriving (Eq)

toSatBit :: Int -> SatBit
toSatBit 0 = Zero
toSatBit 1 = One
toSatBit 2 = DontCare
toSatBit _ = error "toSatBit: Invalid sat bit returned from CUDD"

expand :: SatBit -> [Bool]
expand Zero     = [False]
expand One      = [True]
expand DontCare = [False, True]

