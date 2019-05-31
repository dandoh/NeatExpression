module Main where

import Expression
import Operations
import Prelude hiding ((+), (*))
import Interpreter
import qualified Data.Map as Map
import qualified Data.Array.Unboxed as U

-- ℝ^5 vector
x :: Expression OneD
x = var1d 5 "x"

-- ℝ
r :: Expression R
r = var "r"

-- ℂ^5
xc :: Expression OneDC
xc = var1dc 5 "xc"

-- ℂ
c :: Expression C
c = varc "c"


size5Array = U.listArray (0, 4) [1, 2, 3, 4, 5]
valMap = ValMaps (Map.fromList []) (Map.fromList [("x", size5Array)])

main :: IO ()
main = do
    print x
    let y = x + x
    let z = y + y
    let dotProduct = x `dot` x
    print dotProduct
    print $ eval x valMap
    print $ eval y valMap
    print $ eval dotProduct valMap
--    eval z (Map.fro
--    print xc
--    print c
--    print $ x + x
--    print $ xc + xc
--    print $ r * x
--    print $ r * xc
--    print $ r * c
