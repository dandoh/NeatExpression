# Neat Expression

New type system:
- Phantom types to carry type information
- Using typeclasses as constraints for operation

```haskell
-- | Type representation of Real and Complex num type
--
data R
    deriving (NumType)

data C
    deriving (NumType)

-- | Type representation of vector dimension
--
data Scalar
    deriving (DimensionType)

data One
    deriving (DimensionType)

data Two
    deriving (DimensionType)

data Three
    deriving (DimensionType)

-- | Type classes
--
class NumType rc

class DimensionType d

class (DimensionType d, NumType rc) =>
      Field d rc


class Field d rc =>
      VectorSpace d rc s


class VectorSpace d rc rc =>
      InnerProductSpace d rc


-- | Instances
--
instance (DimensionType d, NumType rc) => Field d rc

instance (Field d rc) => VectorSpace d rc R

instance (Field d C) => VectorSpace d C C

instance (VectorSpace d rc rc) => InnerProductSpace d rc

-- | Shape type:
-- []        --> scalar
-- [n]       --> 1D with size n
-- [n, m]    --> 2D with size n × m
-- [n, m, p] --> 3D with size n × m × p
type Shape = [Int]

-- | Args - list of indices of arguments in the ExpressionMap
--
type Args = [Int]

-- | Data representation of Real and Complex num type
--
data RC
    = Real
    | Complex
    deriving (Show, Eq, Ord)

-- | Shape and RC -> we can reconstruct the type of the Expression
--
type Internal = (Shape, RC, Node)

type ExpressionMap = IntMap Internal

data Expression d rc =
    Expression
        Int -- the index this expression
        ExpressionMap -- all subexpressions
    deriving (Show, Eq, Ord, Typeable)

data Node
    = Var String
    | DVar String
    | Sum Args
    | Mul Args
    | Scale Args --- scalar is in the first
    | Dot Args
    deriving (Show, Eq, Ord)

```

Operation can then be defined:
```haskell
-- | Create primitive expressions
--
var :: String -> Expression Scalar R
var name = Expression h (fromList [(h, node)])
  where
    node = ([], Var name)
    h = hash node

var1d :: Int -> String -> Expression One R
var1d size name = Expression h (fromList [(h, node)])
  where
    node = ([size], Var name)
    h = hash node

var2d :: (Int, Int) -> String -> Expression Two R
var2d (size1, size2) name = Expression h (fromList [(h, node)])
  where
    node = ([size1, size2], Var name)
    h = hash node

-- | Element-wise sum
--
(+) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = expressionNumType e1
    shape = expressionShape e1
    node = Sum numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Element-wise multiplication (like in MATLAB)
--
(.*) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(.*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = expressionNumType e1
    shape = expressionShape e1
    node = Mul numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Scale by scalar
--
(*) :: VectorSpace d rc s
    => Expression Scalar s
    -> Expression d rc
    -> Expression d rc
(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
  where
    numType = expressionNumType e2
    shape = expressionShape e2
    node = Scale numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | Inner product in Inner Product Space
--
(<.>) :: InnerProductSpace d rc
    => Expression d rc
    -> Expression d rc
    -> Expression Scalar rc
(<.>) e1@(Expression n1 mp1) e2@(Expression n2 mp2) = Expression h newMap
  where
    numType = expressionNumType e1
    shape = []
    node = InnerProd numType [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

-- | From R to C two part
-- TODO: more constraint for this operation ? (Field d R, Field d C, ..)
--
(+:) :: (DimensionType d) => Expression d R -> Expression d R -> Expression d C
(+:) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    shape = expressionShape e1
    node = RealImg [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, node)

```
