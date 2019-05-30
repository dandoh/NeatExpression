{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Expression where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM

-- | Scalar and vectors
--
data R

data C

data OneD

data OneDC

-- | Vector space instances
--
class VectorSpace v s

instance VectorSpace OneD R

instance VectorSpace OneDC R

instance VectorSpace OneDC C

instance VectorSpace C R

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

-- | Utilities to get dimension type
--
type family DimType a where
    DimType R = ()
    DimType C = ()
    DimType OneD = Int
    DimType OneDC = Int
    DimType _ = ()

class HaveDimension v where
    getDimension :: Expression v -> DimType v

instance HaveDimension R where
    getDimension (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim0, _) -> ()
            _ -> error "Expression of R but dimension is not Dim0"

instance HaveDimension C where
    getDimension (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim0, _) -> ()
            _ -> error "Expression of C but dimension is not Dim0"

instance HaveDimension OneD where
    getDimension (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim1 size, _) -> size
            _ -> error "Expression of OneD but dimension is not Int"

instance HaveDimension OneDC where
    getDimension (Expression n mp) =
        case IM.lookup n mp of
            Just (Dim1 size, _) -> size
            _ -> error "Expression of OneD but dimension is not Int"

-- | Create primitive expression
--
var :: String -> Expression R
var name = Expression 123 (IM.fromList [(123, (Dim0, Var name))])

varc :: String -> Expression R
varc name = Expression 123 (IM.fromList [(123, (Dim0, Var name))])

var1d :: Int -> String -> Expression OneD
var1d size name = Expression 123 (IM.fromList [(123, (Dim1 size, Var name))])

var1dc :: Int -> String -> Expression OneD
var1dc size name = Expression 123 (IM.fromList [(123, (Dim1 size, Var name))])
