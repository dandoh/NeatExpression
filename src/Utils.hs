{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where

import Data.IntMap.Strict
import Expression
import Hash
import Prelude hiding ((*), (+), lookup)

-- | Auxiliary functions for operations
--
getNumType :: (NumType rc) => Expression d rc -> RC
getNumType (Expression n mp) =
    case lookup n mp of
        Just (_, nt, _) -> nt
        _ -> error "expression not in map"

getShape :: (DimensionType d) => Expression d rc -> Shape
getShape (Expression n mp) =
    case lookup n mp of
        Just (dim, _, _) -> dim
        _ -> error "expression not in map"

ensureSameShape :: (Field d rc) => Expression d rc -> Expression d rc -> a -> a
ensureSameShape e1 e2 after =
    if getShape e1 == getShape e2
        then after
        else error "Ensure same shape failed"
