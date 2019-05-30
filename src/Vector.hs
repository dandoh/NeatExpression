{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vector where

-- |
--
data Scalar

data ScalarC

data OneD

data OneDC

-- |
--
class VectorSpace v s

instance VectorSpace OneD Scalar

instance VectorSpace OneDC Scalar

instance VectorSpace OneDC ScalarC

instance VectorSpace ScalarC Scalar

-- |
--
data Dim
    = Dim0
    | Dim1 Int
    deriving (Show, Eq, Ord)
