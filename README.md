# Just Miscellaneous Haskell programs from my learning

Todo:

 * Stuff involving more of Math and Logic as well as Algorithms
 * Later some stuff on Monads & MTL
 * Stuff on Network & Systems programming
 * Stuff for calling Web API and other RESTful services using Haskell

Some stuff! :+1:

```haskell
-- Getting next elements of the polynomial series
continue :: [Integer] -> [Integer]
continue xs = map last $ iterate nextD differences
         where
         differences = nextD $ genDifs xs

-- Α list of sums of squares
-- ∑(∑n²) = 1² + (1² + 2²) + (1² + 2² + 3²) + ...
--        = 1 + 5 + 14 + 30 + 55...
-- λ> take 10 (continue [1, 5, 14, 30, 55])
-- [91,140,204,285,385,506,650,819,1015,1240]

-- Newton's square root method
newton :: Float -> Float
newton x = until isGoodEnough improve x
       where
       isGoodEnough y = abs (y * y - x) < eps * x
       improve y = (y + x/y) / 2
       until p f z = if p z then x else until p f (f z)
       eps = 0.0000001

-- Pythagorean Triads (x, y, z)
-- 1 <= x,y,z <= n 2x2 < x2 + y2 = z2 ≤ n2 => x ≤ (n/√2)
-- restriction1: x < y
-- restriction2: x & y are coprime
triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) | x <- [1 .. m]
                      , y <- [x+1 .. n]
                      , coprime x y
                      , z <- [y+1 .. n]
                      , x*x + y*y == z*z]
           where
           m = floor (fromIntegral n / sqrt 2)

-- Ramanujan's Taxi Cab numbers
{--
   1729 is the first number which can be expressed as the
   sum of 2 cubes in essentially different ways
   1^3 + 12^3 = 9^3 + 10^3 = 1729
   Here is the version to get second such number
   Define a function that returns a list of all essentially
   different quadruples (a,b,c,d) in the following range
   0 < a,b,c,d ≤ n such that a^3 + b^3 = c^3 +d^3
   restricting the quadruple set to satosfy the below
    a ≤ b and c ≤ d and a < c.
-}
quads :: Int -> [(Int, Int, Int, Int)]
quads n = [(a, b, c, d) | a <- [1 .. n], b <- [a .. n]
                         , c <- [a+1 .. n], d <- [c .. n]
                         , (a^3 + b^3) == (c^3 + d^3)]

-- Getting the nth Taxi Cab number set from the quads helper function
taxiCabSet :: Int -> (Int, Int, Int, Int)
taxiCabSet n
    | n <= 0 = error "No Solution"
    | otherwise = f [1 ..] !! (n-1)
                  where
                  f = L.nub . concatMap quads

-- TaxiCab number and set
taxiCab :: Int -> (Int, [Int])
taxiCab n = (\(a,b,c,d) -> (a^3+b^3, [a,b,c,d])) $ taxiCabSet n
```

And others in my code.
