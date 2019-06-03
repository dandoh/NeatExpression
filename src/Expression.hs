{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Expression where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)

-- | Type representation of Real and Complex num type
--
data R
    deriving (NumType)

data C
    deriving (NumType)

-- | Type representation of vector dimension
--
data Scalar
    deriving (DimensionType)

data One
    deriving (DimensionType)

data Two
    deriving (DimensionType)

data Three
    deriving (DimensionType)

-- | Type classes
--
class NumType rc

class DimensionType d

class (DimensionType d, NumType rc) =>
      Field d rc


class Field d rc =>
      VectorSpace d rc s


class VectorSpace d rc rc =>
      InnerProductSpace d rc


-- | Instances
--
instance (DimensionType d, NumType rc) => Field d rc

instance (Field d rc) => VectorSpace d rc R

instance (Field d C) => VectorSpace d C C

instance (VectorSpace d rc rc) => InnerProductSpace d rc

-- | Shape type:
-- []        --> scalar
-- [n]       --> 1D with size n
-- [n, m]    --> 2D with size n × m
-- [n, m, p] --> 3D with size n × m × p
type Shape = [Int]

-- | Args - list of indices of arguments in the ExpressionMap
--
type Args = [Int]

-- | Data representation of Real and Complex num type
--
data RC
    = Real
    | Complex
    deriving (Show, Eq, Ord)

-- | Internal
-- Shape: Shape of the expression
-- we can reconstruct the type of the Expression
--
type Internal = (Shape, Node)

-- | Hash map of all subexpressions
--
type ExpressionMap = IntMap Internal

-- | Expression with 2 phantom types (dimension and num type)
--
data Expression d rc =
    Expression
        Int -- the index this expression
        ExpressionMap -- all subexpressions
    deriving (Show, Eq, Ord, Typeable)

data Node
    = Var String
    | DVar String
    | Sum RC Args
    | Mul RC Args
    | Scale RC Args --- scalar is in the first
    | Dot RC Args
    deriving (Show, Eq, Ord)

nodeNumType :: Node -> RC
nodeNumType node =
    case node of
        Var _ -> Real
        DVar _ -> Real
        Sum rc _ -> rc
        Mul rc _ -> rc
        Scale rc _ -> rc
        Dot rc _ -> rc
