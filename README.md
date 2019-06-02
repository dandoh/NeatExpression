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
    node = ([], Real, Var name)
    h = hash node

varc :: String -> Expression Scalar C
varc name = Expression h (fromList [(h, node)])
  where
    node = ([], Complex, Var name)
    h = hash node

var1d :: Int -> String -> Expression One R
var1d size name = Expression h (fromList [(h, node)])
  where
    node = ([size], Real, Var name)
    h = hash node

var1dc :: Int -> String -> Expression One C
var1dc size name = Expression h (fromList [(h, node)])
  where
    node = ([size], Complex, Var name)
    h = hash node

-- | Element-wise sum
--

(+) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(+) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = getNumType e1
    shape = getShape e1
    node = Sum [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

-- | Element-wise multiplication
--
(.*) :: (Field d rc) => Expression d rc -> Expression d rc -> Expression d rc
(.*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    numType = getNumType e1
    shape = getShape e1
    node = Mul [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

-- | Scale by scalar
--
(*) :: VectorSpace d rc s => Expression Scalar s -> Expression d rc -> Expression d rc
(*) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    Expression h newMap
  where
    numType = getNumType e2
    shape = getShape e2
    node = Scale [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

-- | Dot product in Inner Product Space
--
dot :: InnerProductSpace d rc => Expression d rc -> Expression d rc -> Expression Scalar rc
dot e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    Expression h newMap
  where
    numType = getNumType e1
    shape = []
    node = Dot [n1, n2]
    (newMap, h) = addEdge (mp1 `union` mp2) (shape, numType, node)

```
