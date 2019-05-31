{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Operations where

import qualified Data.IntMap.Strict as IM
import Expression
import Hash
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

(+) :: Addable a => Expression a -> Expression a -> Expression a
(+) e1@(Expression v1 mp1) e2@(Expression v2 mp2) =
    if getShape e1 == getShape e2
        then Expression res newMap
        else error "add two things different size"
  where
    mergedMap = mp1 `IM.union` mp2
    (newMap, res) = addEdge mergedMap (getDimension e1, Sum (on e1) [v1, v2])

(*) :: VectorSpace v s => Expression s -> Expression v -> Expression v
(*) e1@(Expression v1 mp1) e2@(Expression v2 mp2) = Expression res newMap
  where
    mergedMap = mp1 `IM.union` mp2
    (newMap, res) = addEdge mergedMap (getDimension e2, Sum (on e2) [v1, v2])
