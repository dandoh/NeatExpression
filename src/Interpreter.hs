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
    eval :: Expression d rc -> ValMaps -> Output d rc

-- |
--
instance Evaluable Scalar R where
    eval :: Expression Scalar R -> ValMaps -> Double
    eval e@(Expression n mp) valMap =
        case IM.lookup n mp of
            Just ([], Var name) ->
                case Map.lookup name $ vm0 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([], Sum Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar R
                    subExp2 = Expression node2 mp :: Expression Scalar R
                 in eval subExp1 valMap + eval subExp2 valMap
            Just ([], Mul Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar R
                    subExp2 = Expression node2 mp :: Expression Scalar R
                 in eval subExp1 valMap * eval subExp2 valMap
            Just ([], Dot Real [node1, node2]) ->
                case IM.lookup node1 mp of
                    Just ([size], _) ->
                        let subExp1 = Expression node1 mp :: Expression One R -- shape is [size], so must be One R
                            subExp2 = Expression node2 mp :: Expression One R -- shape is [size], so must be One R
                            lst1 = A.elems $ eval subExp1 valMap
                            lst2 = A.elems $ eval subExp2 valMap
                         in sum $ zipWith (*) lst1 lst2
            _ -> error "expression structure Scalar R is wrong"

-- |
--
instance Evaluable Scalar C where
    eval :: Expression Scalar C -> ValMaps -> DC.Complex Double
    eval e@(Expression n mp) valMap =
        case IM.lookup n mp of
            Just ([], Sum Complex [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar C
                    subExp2 = Expression node2 mp :: Expression Scalar C
                 in eval subExp1 valMap + eval subExp2 valMap
            Just ([], Mul Complex [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar C
                    subExp2 = Expression node2 mp :: Expression Scalar C
                 in eval subExp1 valMap * eval subExp2 valMap
            Just ([], Dot Complex [node1, node2]) ->
                case IM.lookup node1 mp of
                    Just ([size], _) ->
                        let subExp1 = Expression node1 mp :: Expression One C -- shape is [size], so must be One R
                            subExp2 = Expression node2 mp :: Expression One C -- shape is [size], so must be One R
                            lst1 = A.elems $ eval subExp1 valMap
                            lst2 = A.elems $ eval subExp2 valMap
                         in sum $ zipWith (*) lst1 lst2
            _ -> error "expression structure Scalar R is wrong"

-- |
--

instance Evaluable One R where
    eval :: Expression One R -> ValMaps -> Array Int Double
    eval e@(Expression n mp) valMap =
        case IM.lookup n mp of
            Just ([size], Var name) ->
                case Map.lookup name $ vm1 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([size], Sum Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One R
                    subExp2 = Expression node2 mp :: Expression One R
                    lst1 = A.elems $ eval subExp1 valMap
                    lst2 = A.elems $ eval subExp2 valMap
                    lstRes = zipWith (+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Mul Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One R
                    subExp2 = Expression node2 mp :: Expression One R
                    lst1 = A.elems $ eval subExp1 valMap
                    lst2 = A.elems $ eval subExp2 valMap
                    lstRes = zipWith (*) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Scale Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression Scalar R
                    subExp2 = Expression node2 mp :: Expression One R
                    scale = eval subExp1 valMap
                    lst = A.elems $ eval subExp2 valMap
                    lstRes = map (* scale) lst
                 in A.listArray (0, size - 1) lstRes
            _ -> error "expression structure One R is wrong"

-- |
--
instance Evaluable One C where
    eval :: Expression One C -> ValMaps -> Array Int (DC.Complex Double)
    eval e@(Expression n mp) valMap =
        case IM.lookup n mp of
            Just ([size], Sum Complex [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One C
                    subExp2 = Expression node2 mp :: Expression One C
                    lst1 = A.elems $ eval subExp1 valMap
                    lst2 = A.elems $ eval subExp2 valMap
                    lstRes = zipWith (+) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Mul Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression One C
                    subExp2 = Expression node2 mp :: Expression One C
                    lst1 = A.elems $ eval subExp1 valMap
                    lst2 = A.elems $ eval subExp2 valMap
                    lstRes = zipWith (*) lst1 lst2
                 in A.listArray (0, size - 1) lstRes
            Just ([size], Scale Complex [node1, node2]) ->
                undefined

--                let subExp1 = Expression node1 mp :: Expression Scalar R
--                    subExp2 = Expression node2 mp :: Expression One R
--                    scale = eval subExp1 valMap
--                    lst = U.elems $ eval subExp2 valMap
--                    lstRes = map (* scale) lst
--                 in U.listArray (0, size - 1) lstRes
            _ -> error "expression structure One R is wrong"
