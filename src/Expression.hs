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

-- | Fields
--
data RC
    = Real
    | Complex
    deriving (Show, Eq, Ord)

class Property f where
    on :: Expression f -> RC

-- | Vector type
--
data R

data C

data OneD

data OneDC

-- | Constraint
--
class Property v =>
      Piecewise v


class Piecewise v =>
      VectorSpace v s


class VectorSpace v s =>
      DotProductSpace v s
    | v -> s


-- | instances
-- TODO : TypeRep instead of using this Property type class ?
instance Property R where
    on _ = Real

instance Property C where
    on _ = Complex

instance Property OneD where
    on _ = Real

instance Property OneDC where
    on _ = Complex

instance Piecewise R

instance Piecewise C

instance Piecewise OneD

instance Piecewise OneDC

instance VectorSpace R R

instance VectorSpace C R

instance VectorSpace OneD R

instance VectorSpace OneDC R

instance VectorSpace OneDC C

instance DotProductSpace OneD R

instance DotProductSpace OneDC C


-- | Proposed new expression type
--
type ExpressionMap = IntMap (Dim, Node)

type Dim = [Int]

type Args = [Int]

data Expression a =
    Expression
        Int -- the index this expression
        ExpressionMap -- all subexpressions
    deriving (Show, Eq, Ord)

data Node
    = Var String
    | DVar String
    | Sum RC Args
    | Prod RC Args
    | Scale RC Args --- scalar is in the first
    | Dot RC Args
    deriving (Show, Eq, Ord)

getDimension :: Expression a -> [Int]
getDimension (Expression n mp) =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        Nothing -> error "not found node in map"
