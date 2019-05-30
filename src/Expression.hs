{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Expression where

import Data.IntMap
import Vector

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

-- | Create primitive expression
--
var :: String -> Expression Scalar
var name = Expression 123 (fromList [(123, (Dim0, Var name))])

varc :: String -> Expression Scalar
varc name = Expression 123 (fromList [(123, (Dim0, Var name))])

var1d :: Int -> String -> Expression OneD
var1d size name = Expression 123 (fromList [(123, (Dim1 size, Var name))])

var1dc :: Int -> String -> Expression OneD
var1dc size name = Expression 123 (fromList [(123, (Dim1 size, Var name))])
