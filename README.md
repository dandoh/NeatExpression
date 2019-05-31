# Neat Expression
Below are the some insights of **HashedExpression**:
- `Scalar, ScalarC, OneD, OneDC, etc` are just wrappers of the type `Expression`, and `Expression` 
itself doesn't carry information of weather it is `Scalar`, `OneD`, or other kind of `vector` (of course doing so is not
easy, I don't know if that is even possible for `HashedExpression` since it involves a map of `subexpressions` (in this case: `ExpressionEdge`)
which requires them be the same type). (1)
- We want to manipulate `Scalar, OneD, OneDC, etc` like in algebra
with operations like `add`, `multiply`. So we make typeclasses to be able 
to do common things with same operation like this: (2)
    ```haskell
    (+) :: OneD -> OneD -> OneD
    (+) :: TwoD -> TwoD -> TwoD
    (+) :: ThreeD -> ThreeD -> ThreeD
    (*) :: Scalar -> TwoD -> TwoD
    (*) :: Scalar -> OneD -> TwoD
    ...
    ```
    Which requires us to give implementation for every 
    combinations of them. But this is the problem because these types are 
    just wrappers around `Expression`. This leads to lots of duplication in 
    codes, e.g: 
    ```haskell
    instance Num Scalar where
        (Scalar expr1) + (Scalar expr2) =
            let (Expression _ es, (n1, n2)) = merge expr1 expr2
             in case (maybeConst expr1, maybeConst expr2) of
                    (_, _) -> Scalar $ addEdge' es (Op Dim0 Sum [n1, n2])
 
    instance Num OneD where
        (OneD expr1) + (OneD expr2) =
            let (Expression _ es, (n1, n2)) = merge expr1 expr2
             in if getDimE' expr1 == getDimE' expr2
                    then OneD $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                    else error $
                         "can't add 1d vectors of lengths " ++ show (expr1, expr2)
                       
    instance Num TwoD where
       (TwoD expr1) + (TwoD expr2) =
           let (Expression _ es, (n1, n2)) = merge expr1 expr2
            in if getDimE' expr1 == getDimE' expr2
                   then TwoD $ addEdge' es (Op (getDimE' expr1) Sum [n1, n2])
                   else error $
                        "can't add 2d vectors of lengths " ++ show (expr1, expr2)
    ```
    ... and more

- The lack of ability to deal with dimension (shape). So we bite the bullet and 
not implement dependent type for dimension, and just store them in the Expression. The problem 
is that when we have a `OneD`, and we know for sure its dimension (shape) has the type Int. Yet, 
if we want to get that Int, we still have to get the `Dim` in the `Expression` and then pattern-match it (3)
    ```haskell
    case dim of 
        Dim1 sz -> doSomething
        _ -> error "we have to write this"
  
    ```

So the idea is to port to a new style to solve those problems:

- (1) Use phantom type. Even if we don't have any `vector` type in the right hand side of Expression, we 
can still put a phantom type in the left hand side. Doing this, we can carry information of the `vector` type of 
`Expression` around. (I have only done this so far, and this type is completely equivalent with our current `Expression` in `HashedExpression`)
  . Morever, now `R, C, OneD, OneDC` only work in type-level but not term-level:

    ```haskell
    type Internal = IntMap (Dim, Node)

    data Expression a =
        Expression
            Int -- the final product of this expression
            (IntMap (Dim, Node)) -- all subexpressions, including vars
        deriving (Show, Eq, Ord)

    data Node
        = Var String
        | DVar String
        | Op Operation
        deriving (Show, Eq, Ord)

    data Operation
        = Sum Int Int
        | Prod Int Int
        deriving (Show, Eq, Ord)
      
    ```
    ```haskell
    data R
        deriving (Addable)

    data C
        deriving (Addable)

    data OneD
        deriving (Addable)

    data OneDC
        deriving (Addable)

    -- | Real Vector Space
    --
    class HasShape v =>
          Addable v


    class Addable v =>
          RealVectorSpace v s
        | v -> s


    instance RealVectorSpace R R

    instance RealVectorSpace C R

    instance RealVectorSpace OneD R

    instance RealVectorSpace OneDC R
    ```
   Now we can do like this (try `stack ghci` this project`):
   ```haskell
    > let x = var "x"
    > x
    Expression 120 (fromList [(120,(Dim0,Var "x"))])
    > :t x
    x :: Expression R
    > let x1d = var "x1d"
    > x1d
    Expression 164764917179 (fromList [(164764917179,(Dim1 10,Var "x1d"))])
    > :t x1d
    x1d :: Expression OneD
    ```

- (2) Now we can use `typeclasses` as `constraints` and write the function once (this make sense 
since `OneD, OneDC` only has meaning in the type-level):
    ```haskell
    (|+|) :: Addable a => Expression a -> Expression a -> Expression a
    e1@(Expression v1 mp1) |+| e2@(Expression v2 mp2) =
        if sameShape e1 e2
            then Expression res newMap
            else error "add 2 vector with different shape"
      where
        mergedMap = mp1 `IM.union` mp2
        op = Sum v1 v2
        (newMap, res) = addEdge mergedMap (getDimension e1, Op op)


    (|*|) :: RealVectorSpace v s => Expression s -> Expression v -> Expression v
    e1@(Expression v1 mp1) |*| e2@(Expression v2 mp2) =
        Expression res newMap
      where
        mergedMap = mp1 `IM.union` mp2
        op = Prod v1 v2
        (newMap, res) = addEdge mergedMap (getDimension e2, Op op)
    ```
  As a result: 
  ```haskell
  > let r = var "r"         --> Expression R
  > let c = varc "c"        --> Expression C
  > let x = var1d 10 "x"    --> Expression OneD
  > let xc = var1dc 15 "xc" --> Expression OneDC
  > x |+| x
  Expression 8185171 (fromList [(120,(Dim1 10,Var "x")),(8185171,(Dim1 10,Op (Sum 120 120)))])
  > :t x |+| x 
  x |+| x :: Expression OneD
  > :t   
  > :t r |*| x
  r |*| x :: Expression OneD
  > :t r |*| c
  r |*| c :: Expression C
  > :t r |*| xc
  r |*| xc :: Expression OneDC
  > :t c |*| x
  <interactive>:1:1: error:
    • Couldn't match type ‘R’ with ‘C’
        arising from a functional dependency between:
          constraint ‘RealVectorSpace OneD C’ arising from a use of ‘|*|’
          instance ‘RealVectorSpace OneD R’
            at /Users/Dandoh/TypedExpression/src/Expression.hs:47:10-31
    • In the expression: c |*| x

  ```
  
- (3) Now because we have type information of the `Expression`, we can now write functions to directly 
get the dimension thanks to `-XTypeFamilies`

    ```haskell
    > :t getShape
    getShape :: HasShape v => Expression v -> Shape v
    > getShape r
    ()
    > getShape x
    10 
    ```
