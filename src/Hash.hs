{-# LANGUAGE MultiParamTypeClasses #-}

module Hash where

import Expression

-- |
--
class HasHash a where
    hash :: a -> Int

-- | Helper hash functions, copy from HashedExpression
--
moveBase :: Char -> Int -> Int
moveBase c hash = hash * 40591 + fromEnum c

argHash :: [Int] -> Int
argHash (arg:args) = arg + 31 * argHash args
argHash [] = 0

-- | Instances
--
instance HasHash Operation where
    hash op =
        case op of
            Sum node1 node2 -> (1 + argHash [node1, node2]) * 2131

instance HasHash Node where
    hash node =
        case node of
            Var name -> foldr moveBase 0 name
            DVar name -> foldr moveBase 1123 name
            Op op -> hash op
