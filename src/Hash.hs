{-# LANGUAGE MultiParamTypeClasses #-}

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

-- | Instances
--
instance HasHash Operation where
    hash op =
        case op of
            Sum node1 node2 -> (1 + argHash [node1, node2]) * 2131
            Prod node1 node2 -> (1 + argHash [node1, node2]) * 2437

instance HasHash Node where
    hash node =
        case node of
            Var name -> foldr moveBase 0 name
            DVar name -> foldr moveBase 1123 name
            Op op -> hash op

-- |
--
data IsClash
    = IsClash
    | IsDuplicate Int
    | IsNew Int
    deriving (Eq, Show, Ord)

isClash :: Internal -> Node -> Int -> IsClash
isClash mp newNode newHash =
    case IM.lookup newHash mp of
        Nothing -> IsNew newHash
        Just (_, old) ->
            if old == newNode
                then IsDuplicate newHash
                else IsClash

addEdge :: Internal -> (Dim, Node) -> (Internal, Int)
addEdge mp (dim, node) =
    case dropWhile (== IsClash) $ map (isClash mp node) . rehash $ hash node of
        (IsDuplicate h:_) -> (mp, h)
        (IsNew h:_) -> (IM.insert h (dim, node) mp, h)
        _ -> error "addEdge everything clashed!"
