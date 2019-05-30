{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Vector where

import qualified Data.Complex as DC

-- | Two type of scalars - real and complex
--
type C = DC.Complex Double

type R = Double

-- | Declare Scalar instances
--
class Scalar s

instance Scalar C

instance Scalar R

-- | Types of vectors
--
data OneD c

data TwoD c

data ThreeD c
