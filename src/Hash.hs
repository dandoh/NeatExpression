{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hash where

import qualified Data.IntMap.Strict as IM
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

rehash :: Int -> [Int]
rehash x = x : [x + (241 + x * 251) * i | i <- [1 ..]]

instance HasHash Internal where
    hash (shape, rc, node) = hash node * argHash shape * hash rc

instance HasHash RC where
    hash Real = 423
    hash Complex = 451

instance HasHash Node where
    hash node =
        case node of
            Var name -> foldr moveBase 0 name
            DVar name -> foldr moveBase 1123 name
            Sum args -> (1 + argHash args) * 2131
            Mul args -> (1 + argHash args) * 2437
            Scale args -> (1 + argHash args) * 3343
            Dot args -> (1 + argHash args) * 3187

-- |
--
data HashOutcome
    = IsClash
    | IsDuplicate Int
    | IsNew Int
    deriving (Eq, Show, Ord)

hashOutcome :: ExpressionMap -> Internal -> Int -> HashOutcome
hashOutcome mp new newHash =
    case IM.lookup newHash mp of
        Nothing -> IsNew newHash
        Just old ->
            if old == new
                then IsDuplicate newHash
                else IsClash

addEdge :: ExpressionMap -> Internal -> (ExpressionMap, Int)
addEdge mp e =
    case dropWhile (== IsClash) $ map (hashOutcome mp e) $ rehash $ hash e of
        (IsDuplicate h:_) -> (mp, h)
        (IsNew h:_) -> (IM.insert h e mp, h)
        _ -> error "addEdge everything clashed!"
