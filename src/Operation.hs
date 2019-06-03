{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Operation where

import Data.IntMap.Strict
import Expression
import Hash
import Prelude hiding ((*), (+))
import Utils

-- | Create primitive expressions
--
var :: String -> Expression Scalar R
var name = Expression h (fromList [(h, node)])
  where
    node = ([], Var name)
    h = hash node

var1d :: Int -> String -> Expression One R
var1d size name = Expression h (fromList [(h, node)])
  where
    node = ([size], Var name)
    h = hash node

var2d :: (Int, Int) -> String -> Expression Two R
var2d (size1, size2) name = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2], Var name)
    h = hash node

-- | Element-wise sum
--
(+) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = expressionNumType e1
    shape = expressionShape e1
    node = Sum numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Element-wise multiplication (like in MATLAB)
--
(.*) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(.*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = expressionNumType e1
    shape = expressionShape e1
    node = Mul numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Scale by scalar
--
(*) :: VectorSpace d rc s
    => Expression Scalar s
    -> Expression d rc
    -> Expression d rc
(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
  where
    numType = expressionNumType e2
    shape = expressionShape e2
    node = Scale numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Dot product in Inner Product Space
--
dot :: InnerProductSpace d rc
    => Expression d rc
    -> Expression d rc
    -> Expression Scalar rc
dot e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
  where
    numType = expressionNumType e1
    shape = []
    node = Dot numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | From R to C two part
-- TODO: more constraint for this operation ? (Field d R, Field d C, ..)
--
(+:) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d C
(+:) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    shape = expressionShape e1
    node = RealImg [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)
