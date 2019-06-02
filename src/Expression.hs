{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Expression where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM

-- | Data representation of Real and Complex num type
--
data RC
    = Real
    | Complex
    deriving (Show, Eq, Ord)

-- | Type representation of Real and Complex num type
--
data R
    deriving (NumType)

data C
    deriving (NumType)

-- | Type representation of vector dimension
--
data One
    deriving (DimensionType)

data Two
    deriving (DimensionType)

data Three
    deriving (DimensionType)

-- | Constraints
--
class NumType n

class DimensionType d

class Field d n

class VectorSpace d n s

-- | Instances
--

instance (DimensionType d, DimensionType n) => Field d n

instance (Field d n) => VectorSpace d n R

instance (Field d n, n ~ C) => VectorSpace d n C

-- | Proposed new expression type
--


type Dim = [Int]

type Args = [Int]

type Internal = (Dim, RC, Node)

type ExpressionMap = IntMap Internal

data Expression d n =
    Expression
        Int -- the index this expression
        ExpressionMap -- all subexpressions
    deriving (Show, Eq, Ord)

data Node
    = Var String
    | DVar String
    | Sum Args
    | Prod Args
    | Scale Args --- scalar is in the first
    | Dot Args
    deriving (Show, Eq, Ord)
