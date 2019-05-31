{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Operations
    ( var
    , var1d
    , var1dc
    , varc
    , (+)
    , (*)
    ) where

import qualified Data.IntMap.Strict as IM
import Expression
import Hash
import Prelude hiding ((*), (+))
import Utils

-- | Create primitive expression
--
var :: String -> Expression R
var name = Expression h (IM.fromList [(h, ([], Var name))])
  where
    h = hash $ Var name

varc :: String -> Expression C
varc name = Expression h (IM.fromList [(h, ([], Var name))])
  where
    h = hash $ Var name

var1d :: Int -> String -> Expression OneD
var1d size name = Expression h (IM.fromList [(h, ([size], Var name))])
  where
    h = hash $ Var name

var1dc :: Int -> String -> Expression OneDC
var1dc size name = Expression h (IM.fromList [(h, ([size], Var name))])
  where
    h = hash $ Var name

-- | Operations
--
getShape :: Expression a -> Dim
getShape (Expression n mp) =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        Nothing -> error "get shape node not found"

(+) :: Piecewise a => Expression a -> Expression a -> Expression a
(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    if getShape e1 == getShape e2
        then Expression res newMap
        else error "add two things different size"
  where
    mergedMap = mp1 `IM.union` mp2
    (newMap, res) = addEdge mergedMap (getDimension e1, Sum (on e1) [n1, n2])

(*) :: Piecewise a => Expression a -> Expression a -> Expression a
(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression res newMap
  where
    mergedMap = mp1 `IM.union` mp2
    (newMap, res) = addEdge mergedMap (getDimension e2, Prod (on e2) [n1, n2])

scale :: VectorSpace v s => Expression s -> Expression v -> Expression v
scale e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression res newMap
  where
    mergedMap = mp1 `IM.union` mp2
    (newMap, res) = addEdge mergedMap (getDimension e2, Scale (on e2) [n1, n2])

dot :: DotProductSpace v s => Expression v -> Expression v -> Expression s
dot e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression res newMap
  where
    mergedMap = mp1 `IM.union` mp2
    (newMap, res) = addEdge mergedMap (getDimension e2, Dot (on e1) [n1, n2]) -- TODO : (on e1) is this right?
