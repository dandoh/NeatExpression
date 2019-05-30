{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Operations where

import qualified Data.IntMap.Strict as IM
import Expression



-- |
--
ensureSameShape :: Expression a -> Expression a -> Bool
ensureSameShape x y = getDimension x == getDimension y

-- |
--

(|+|) :: VectorSpace a b => Expression a -> Expression a -> Expression a
(|+|) (Expression v1 mp1) (Expression v2 mp2) = undefined
  where
    mergedMap = mp1 `IM.union` mp2
    op = Sum v1 v2
    newMap = IM.insert 456 (Dim0, Op op) mergedMap
