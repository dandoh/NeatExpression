{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Expression where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM

-- | Scalar and vectors
--
data R
    deriving (Additive)

data C
    deriving (Additive)

data OneD
    deriving (Additive)

data OneDC
    deriving (Additive)

-- | Real Vector Space
--
class Additive v

class Additive v =>
      RealVectorSpace v s
    | v -> s


instance RealVectorSpace R R

instance RealVectorSpace C R

instance RealVectorSpace OneD R

instance RealVectorSpace OneDC R

-- | Proposed new expression type
--
data Expression a =
    Expression
        Int -- the final product of this expression
        (IntMap (Dim, Node)) -- all subexpressions, including vars
    deriving (Show, Eq, Ord)

data Node
    = Var String
    | DVar String
    | Op Operation
    deriving (Show, Eq, Ord)

data Operation =
    Sum Int Int
    deriving (Show, Eq, Ord)

-- | Dimension type
--
data Dim
    = Dim0
    | Dim1 Int
    deriving (Show, Eq, Ord)

getDimension :: Expression a -> Dim
getDimension (Expression n mp) =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        Nothing -> error "no dimension of expression"

-- | Utilities to get shape
--
type family Shape a where
    Shape R = ()
    Shape C = ()
    Shape OneD = Int
    Shape OneDC = Int
    Shape _ = ()

class Eq (Shape v) =>
      HasShape v
    where
    getShape :: Expression v -> Shape v

instance HasShape R where
    getShape (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim0, _) -> ()
            _ -> error "Expression of R but dimension is not Dim0"

instance HasShape C where
    getShape (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim0, _) -> ()
            _ -> error "Expression of C but dimension is not Dim0"

instance HasShape OneD where
    getShape (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim1 size, _) -> size
            _ -> error "Expression of OneD but dimension is not Int"

instance HasShape OneDC where
    getShape (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim1 size, _) -> size
            _ -> error "Expression of OneD but dimension is not Int"

sameShape :: HasShape a => Expression a -> Expression a -> Bool
sameShape x y = getShape x == getShape y

