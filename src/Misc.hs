-- Miscellaneous short functions
module Misc where

import qualified Data.List as L
import           Prelude   hiding (until)


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

-- Helper functions divisors and coprime
divisors :: Int -> [Int]
divisors x = [d | d <- [2 .. x-1], x `mod` d == 0]

coprime :: Int -> Int -> Bool
coprime a b = disjoint (divisors a) (divisors b)
        where
        -- disjoint takes 2 lists in ascending order and
        -- determines if they have an element in common
        disjoint :: [Int] -> [Int] -> Bool
        disjoint [] [] = True
        disjoint _ [] = True
        disjoint [] _ = True
        disjoint (x : xs) (y : ys)
            | x < y = disjoint xs (y : ys)
            | x == y = False
            | x > y = disjoint (x : xs) ys

-- position of a value in a List
-- if value is in the list, index is returned, otherwise -1
position :: (Eq a) => a -> [a] -> Int
position x xs = head ([a | (a, b) <- zip [0 .. ] xs, b == x] ++ [-1])

-- Merge Sort
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
           where
           (ys, zs) = halve xs

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
           where
           n = length xs `div` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge ys [] = ys
merge [] zs = zs
merge (y : ys) (z : zs)
    | y <= z = y : merge ys (z : zs)
    | otherwise = z : merge (y : ys) zs

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
