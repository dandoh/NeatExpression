module Main where

import Expression
import Operations

x :: Expression OneD
x = var1d 5 "x"

c :: Expression R
c = var "c"

z = c |*| x

main :: IO ()
main = print $ z
