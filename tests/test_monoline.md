```haskell
> -- define a new type: 
> data Point = Point Float Float deriving (Show)
> -- variable of type Point:
> let point = (Point 3 4)
> point
Point 3.0 4.0
> :t point
point :: Point
> -- 'Point' is like a function:
> :t Point
Point :: Float -> Float -> Point
> -- a function acting on 'Point' variables:
> :{
> let squareNorm :: Point -> Float
>     squareNorm (Point x y) = x^2+y^2
> :}
> squareNorm point
25.0
```
