{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Operation where

import Data.IntMap.Strict
import Expression
import Hash
import Prelude hiding ((*), (+))

-- | Create primitive expression
--
--var :: String -> Expression Scalar R
--var name = Expression h (IM.fromList [(h, ([], Var name))])
--  where
--    h = hash $ Var name
--
--varc :: String -> Expression Scalar C
--varc name = Expression h (IM.fromList [(h, ([], Var name))])
--  where
--    h = hash $ Var name
--
--var1d :: Int -> String -> Expression One R
--var1d size name = Expression h (IM.fromList [(h, ([size], Var name))])
--  where
--    h = hash $ Var name
--
--var1dc :: Int -> String -> Expression One C
--var1dc size name = Expression h (IM.fromList [(h, ([size], Var name))])
--  where
--    h = hash $ Var name
-- | Operations
--
ensureSameShape :: (Field d rc) => Expression d rc -> Expression d rc -> a -> a
ensureSameShape e1 e2 after =
    if getShape e1 == getShape e2
        then after
        else error "Ensure same shape failed"

-- | Element-wise sum
--

(+) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = getNumType e1
    shape = getShape e1
    node = Sum [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

-- | Element-wise multiplication
--
(.*) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(.*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = getNumType e1
    shape = getShape e1
    node = Mul [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

-- | Scale by scalar
--
(*) :: VectorSpace d rc s => Expression Scalar s -> Expression d rc -> Expression d rc
(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    Expression h newMap
  where
    numType = getNumType e2
    shape = getShape e2
    node = Scale [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

-- | Dot product in Inner Product Space
--
dot :: InnerProductSpace d rc => Expression d rc -> Expression d rc -> Expression Scalar rc
dot e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    Expression h newMap
  where
    numType = getNumType e1
    shape = []
    node = Dot [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

