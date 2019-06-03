module EvalSpec where

import Data.Array
import Test.Hspec
import Interpreter
import Expression
import Operation
import Prelude hiding ((+), (*))
import qualified Prelude
import Data.Complex

spec :: Spec
spec = do
    describe "eval basics" $ do
        specify "real scalar 01" $ do
            let x = var "x"
                y = var "y"
                valMap = subs [("x", 1), ("y", 3)] []
            eval valMap x `shouldBe` 1
            eval valMap y `shouldBe` 3
            eval valMap (x + y) `shouldBe` 4
            eval valMap (x * y) `shouldBe` 3
            eval valMap (x .* y) `shouldBe` 3
            eval valMap (x <.> y) `shouldBe` 3
        specify "real scalar 02" $ do
            let x = var "x"
                y = var "y"
                valMap = subs [("x", 100), ("y", 300)] []
            eval valMap x `shouldBe` 100
            eval valMap y `shouldBe` 300
            eval valMap (x + y) `shouldBe` 400
            eval valMap (x * y) `shouldBe` 30000
            eval valMap (x .* y) `shouldBe` 30000
            eval valMap (x <.> y) `shouldBe` 30000
        specify "complex scalar 01" $ do
            let x = var "x"
                y = var "y"
                z1 = x +: y
                z2 = y +: x
                valMap = subs [("x", 1), ("y", 3)] []
            eval valMap z1 `shouldBe` (1 :+ 3)
            eval valMap z2 `shouldBe` (3 :+ 1)
            eval valMap (y * z1) `shouldBe` 3 Prelude.* (1 :+ 3)
            eval valMap (z2 * z1) `shouldBe` (3 :+ 1) Prelude.* (1 :+ 3)
            eval valMap (z2 .* z1) `shouldBe` (3 :+ 1) Prelude.* (1 :+ 3)
            eval valMap (z2 <.> z1) `shouldBe` (3 :+ 1) Prelude.* (1 :+ 3)
        specify "real 1d 01" $ do
            let x = var1d 4 "x"
                y = var1d 4 "y"
                s = var "s"
                valMap = subs [("s", 3)] [("x", listArray (0, 3) [1, 1, 1, 1]), ("y", listArray (0, 3) [1, 1, 1, 1])]
            elems (eval valMap x) `shouldBe` [1, 1, 1, 1]
            elems (eval valMap (x + y)) `shouldBe` [2, 2, 2, 2]
            elems (eval valMap (s * x)) `shouldBe` [3, 3, 3, 3]
            elems (eval valMap (x .* y)) `shouldBe` [1, 1, 1, 1]
            eval valMap (x <.> y) `shouldBe` 4
