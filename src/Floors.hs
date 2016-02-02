-- Implementation of floor function
-- a <= x < b

floor1 :: Float -> Integer
floor1 x = if x < 0
              then until (`leq` x) (subtract 1) (-1)
              else until (x `lt`) (+1) 1 - 1

leq :: Integer -> Float -> Bool
a `leq` x = fromInteger a <= x

lt :: Float -> Integer -> Bool
x `lt` b = x < fromInteger b

-- Efficient version of floor function


floor2 :: Float -> Integer
floor2 x = fst (until unit (shrink x) (bound x))
           where
           unit (a, b) = (b - a == 1)

type Interval = (Integer, Integer)

shrink :: Float -> Interval -> Interval
shrink x (a, b) = if p `leq` x
                     then (p, b)
                     else (a, p)
                     where
                     p = choose (a, b)

choose :: Interval -> Integer
choose (a, b) = (a+b) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

lower :: Float -> Integer
lower x = until (`leq` x) (*2) (-1)

upper :: Float -> Integer
upper x = until (x `lt`) (*2) 1
