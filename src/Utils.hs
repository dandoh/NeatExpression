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
expressionNumType :: (NumType rc) => Expression d rc -> RC
expressionNumType (Expression n mp) =
    case lookup n mp of
        Just (_, node) -> nodeNumType node
        _ -> error "expression not in map"

expressionShape :: (DimensionType d) => Expression d rc -> Shape
expressionShape (Expression n mp) =
    case lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"

ensureSameShape :: (Field d rc) => Expression d rc -> Expression d rc -> a -> a
ensureSameShape e1 e2 after =
    if expressionShape e1 == expressionShape e2
        then after
        else error "Ensure same shape failed"
