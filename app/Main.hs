module Main where

import Expression
import Operations
import Prelude hiding ((+), (*))

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

main :: IO ()
main = do
    print x
    let y = x + x
    let z = y + y
    print y
    print z
--    print xc
--    print c
--    print $ x + x
--    print $ xc + xc
--    print $ r * x
--    print $ r * xc
--    print $ r * c
