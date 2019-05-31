{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Interpreter where

import qualified Data.Array.Unboxed as U
import Data.Complex as DC
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as Map
import Expression

type Array i e = U.UArray i e

type family Output a where
    Output R = Double
    Output C = DC.Complex Double
    Output OneD = Array Int Double
    Output OneDC = Array Int (DC.Complex Double)

-- |
--
data ValMaps =
    ValMaps
        { vm0 :: Map String Double
        , vm1 :: Map String (Array Int Double)
        }
    deriving (Eq, Show, Ord)

-- |
--
--class Sum a
class Evaluable a where
    eval :: Expression a -> ValMaps -> Output a

-- |
--
instance Evaluable R where
    eval :: Expression R -> ValMaps -> Double
    eval e@(Expression n mp) valMap =
        case IM.lookup n mp of
            Just ([], Var name) ->
                case Map.lookup name $ vm0 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([], Sum Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression R
                    subExp2 = Expression node2 mp :: Expression R
                 in eval subExp1 valMap + eval subExp2 valMap
            Just ([], Prod Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression R
                    subExp2 = Expression node2 mp :: Expression R
                 in eval subExp1 valMap * eval subExp2 valMap
            Just ([], Dot Real [node1, node2]) ->
                case IM.lookup node1 mp of
                    Just ([size], _)
                        -- because dim is [size], they must be OneD
                     ->
                        let subExp1 = Expression node1 mp :: Expression OneD
                            subExp2 = Expression node2 mp :: Expression OneD
                            lst1 = U.elems $ eval subExp1 valMap
                            lst2 = U.elems $ eval subExp2 valMap
                         in product $ zipWith (+) lst1 lst2
            _ -> error "expression structure is wrong"

instance Evaluable OneD where
    eval :: Expression OneD -> ValMaps -> Array Int Double
    eval e@(Expression n mp) valMap =
        case IM.lookup n mp of
            Just ([], Var name) ->
                case Map.lookup name $ vm1 valMap of
                    Just val -> val
                    _ -> error "no value associated with the variable"
            Just ([size], Sum Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression OneD
                    subExp2 = Expression node2 mp :: Expression OneD
                    lst1 = U.elems $ eval subExp1 valMap
                    lst2 = U.elems $ eval subExp2 valMap
                    lstRes = zipWith (+) lst1 lst2
                 in U.listArray (0, size - 1) lstRes
            Just ([size], Prod Real [node1, node2]) ->
                let subExp1 = Expression node1 mp :: Expression OneD
                    subExp2 = Expression node2 mp :: Expression OneD
                    lst1 = U.elems $ eval subExp1 valMap
                    lst2 = U.elems $ eval subExp2 valMap
                    lstRes = zipWith (*) lst1 lst2
                 in U.listArray (0, size - 1) lstRes
            _ -> error "expression structure is wrong"
