{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Operations where

import qualified Data.IntMap.Strict as IM
import Expression
import Hash

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

-- |
--
(|+|) :: Additive a => Expression a -> Expression a -> Expression a
(|+|) (Expression v1 mp1) (Expression v2 mp2) = undefined
  where
    mergedMap = mp1 `IM.union` mp2
    op = Sum v1 v2
    newMap = IM.insert 456 (Dim0, Op op) mergedMap
