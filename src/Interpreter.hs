{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter where

import qualified Data.Array as A
import Data.Complex as DC
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as Map
import Expression
import Utils

type Array i e = A.Array i e

type family Output d rc where
    Output Scalar R = Double
    Output Scalar C = DC.Complex Double
    Output One R = Array Int Double
    Output One C = Array Int (DC.Complex Double)

-- |
--
data ValMaps =
    ValMaps
        { vm0 :: Map String Double
        , vm1 :: Map String (Array Int Double)
        }
    deriving (Eq, Show, Ord)

class Evaluable d rc where
    eval :: ValMaps -> Expression d rc -> Output d rc

-- |
--
instance Evaluable Scalar R where
    eval :: ValMaps -> Expression Scalar R -> Double
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([], Var name) ->
                case Map.lookup name $ vm0 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([], Sum Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar R
                    subExp2 = Expression node2 mp :: Expression Scalar R
                 in eval valMap subExp1 + eval valMap subExp2
            Just ([], Mul Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar R
                    subExp2 = Expression node2 mp :: Expression Scalar R
                 in eval valMap subExp1 * eval valMap subExp2
            Just ([], Dot Real [node1, node2]) ->
                case IM.lookup node1 mp of
                    Just ([size], _) ->
                        let subExp1 = Expression node1 mp :: Expression One R -- shape is [size], so must be One R
                            subExp2 = Expression node2 mp :: Expression One R -- shape is [size], so must be One R
                            lst1 = A.elems $ eval valMap subExp1
                            lst2 = A.elems $ eval valMap subExp2
                         in sum $ zipWith (*) lst1 lst2
            _ -> error "expression structure Scalar R is wrong"

-- |
--
instance Evaluable Scalar C where
    eval :: ValMaps -> Expression Scalar C -> DC.Complex Double
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([], Sum Complex [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar C
                    subExp2 = Expression node2 mp :: Expression Scalar C
                 in eval valMap subExp1 + eval valMap subExp2
            Just ([], Mul Complex [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar C
                    subExp2 = Expression node2 mp :: Expression Scalar C
                 in eval valMap subExp1 * eval valMap subExp2
            Just ([], Dot Complex [node1, node2]) ->
                case IM.lookup node1 mp of
                    Just ([size], _) ->
                        let subExp1 = Expression node1 mp :: Expression One C -- shape is [size], so must be One R
                            subExp2 = Expression node2 mp :: Expression One C -- shape is [size], so must be One R
                            lst1 = A.elems $ eval valMap subExp1
                            lst2 = A.elems $ eval valMap subExp2
                         in sum $ zipWith (*) lst1 lst2
            _ -> error "expression structure Scalar R is wrong"

-- |
--
instance Evaluable One R where
    eval :: ValMaps -> Expression One R -> Array Int Double
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([size], Var name) ->
                case Map.lookup name $ vm1 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([size], Sum Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One R
                    subExp2 = Expression node2 mp :: Expression One R
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Mul Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One R
                    subExp2 = Expression node2 mp :: Expression One R
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (*) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Scale Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar R
                    subExp2 = Expression node2 mp :: Expression One R
                    scale = eval valMap subExp1
                    lst = A.elems $ eval valMap subExp2
                    lstRes = map (* scale) lst
                 in A.listArray (0, size - 1) lstRes
            _ -> error "expression structure One R is wrong"

-- |
--
instance Evaluable One C where
    eval :: ValMaps -> Expression One C -> Array Int (DC.Complex Double)
    eval valMap e@(Expression n mp) =
        case IM.lookup n mp of
            Just ([size], Sum Complex [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One C
                    subExp2 = Expression node2 mp :: Expression One C
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Mul Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One C
                    subExp2 = Expression node2 mp :: Expression One C
                    lst1 = A.elems $ eval valMap subExp1
                    lst2 = A.elems $ eval valMap subExp2
                    lstRes = zipWith (*) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Scale Complex [node1, node2]) ->
                let subExp2 = Expression node2 mp :: Expression One C
                    lst = A.elems $ eval valMap subExp2
                    scale =
                        case nodeNumType . retrieveNode mp $ node1 of
                            Real ->
                                fromReal . eval valMap $
                                (Expression node1 mp :: Expression Scalar R)
                            Complex ->
                                eval
                                    valMap
                                    (Expression node1 mp :: Expression Scalar C)
                 in A.listArray (0, size - 1) $ map (* scale) lst
            _ -> error "expression structure One R is wrong"
