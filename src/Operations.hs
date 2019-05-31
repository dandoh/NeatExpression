{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Operations where

import qualified Data.IntMap.Strict as IM
import Expression
import Hash
import Utils

-- | Create primitive expression
--
var :: String -> Expression R
var name = Expression h (IM.fromList [(h, (Dim0, Var name))])
  where
    h = hash $ Var name

var1d :: Int -> String -> Expression OneD
var1d size name = Expression h (IM.fromList [(h, (Dim1 size, Var name))])
  where
    h = hash $ Var name

-- |
--
(|+|) ::
       (Additive a, HasShape a) => Expression a -> Expression a -> Expression a
e1@(Expression v1 mp1) |+| e2@(Expression v2 mp2) =
    if sameShape e1 e2
        then undefined
        else error "add 2 vector with different shape"
  where
    mergedMap = mp1 `IM.union` mp2
    op = Sum v1 v2
    newMap = IM.insert 456 (Dim0, Op op) mergedMap
