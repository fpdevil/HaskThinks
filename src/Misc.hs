-- Miscellaneous short functions
module Misc where

import qualified Data.List as L
import           Prelude   hiding (until)


-- Newton's square root method
newton :: Float -> Float
newton x = until isGoodEnough improve x
       where
       isGoodEnough y = abs (y * y - x) < eps * x
       improve y      = (y + x/y) / 2
       until p f z    = if p z then x else until p f (f z)
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
        disjoint _ []  = True
        disjoint [] _  = True
        disjoint (x : xs) (y : ys)
            | x < y  = disjoint xs (y : ys)
            | x == y = False
            | x > y  = disjoint (x : xs) ys

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

----------------------------------------------------------------
-- PRIMALITY TESTS
----------------------------------------------------------------
-- Least natural number greater than 1 that divides n
{--
-- 1. if n > 1, then ld(n) is Prime number
-- 2. if n > 1 and n is not Prime, then (ld(n))^2 <= n

Proof By Contradiction:
if c = ld(n) is not prime then there exists natural numbers
a and b such that c = a*b and also 1 < a < c. But since a
divides n, this contradicts the fact that c is the smallest
number to divide n. Hence (1) is TRUE.

Suppose if n > 1 , n is not a prime number and p = ld(n).
Then there is a natural number a > 1 with p * a = n. Thus
a also divides n. Since p is the smallest divisor of n with
p > 1, we have p <= a
Hence p^2 <= p*a = n which indicates (ld(n))^2 <= n
--}
-- Helper function for checking if a number divides another
divides :: (Integral a) => a -> a -> Bool
divides 0 _     = error "Divion By Zero"
divides den num = num `rem` den == 0

-- Least Divisor Function ldf(k)(n)
-- This is the least divisor starting from a value k
-- with k <= n. ldf(k)(n) gives the least divisor of
-- n which is >= k.
-- Also ld(n) = ldf(2)(n)
ldf :: (Integral a) => a -> a -> a
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

-- ld(n)
-- ld looks for a prime divisor of n by checking k|n for all k with
-- 2 <= k <= √n
ld :: (Integral a) => a -> a
ld = ldf 2

-- First primality check
primality1 :: (Integral a) => a -> Bool
primality1 n | n < 1     = error "Not a positive integer"
             | n == 1    = False
             | otherwise = ld n == n

-- Factors of number (Prime Factors ???)
factors :: (Integral a) => a -> [a]
factors x | x < 1     = error "Negative number"
          | x == 1    = []
          | otherwise = p : factors (x `div` p)
          where p = ld x

-- filtering primes from the infinite list [1 .. ]
primes1 :: (Integral a) => [a]
primes1 = filter primality1 [1..]

-- The function ldf used in the definition of ld looks for a
-- prime divisor of n by checking k|n for all k with 2 <= k <= √n.
-- In fact, it is enough to check p|n for the primes p with
-- 2 <=  p <=  √n
ldp :: (Integral a) => a -> a
ldp = ldpf primes1

ldpf :: (Integral a) => [a] -> a -> a
ldpf [] _ = 0
ldpf (p : ps) x
    | x `rem` p == 0 = p
    | p^2 > x        = x
    | otherwise      = ldpf ps x

primes2 :: (Integral a) => [a]
primes2 = 2 : filter primality2 [3 ..]

primality2 :: (Integral a) => a -> Bool
primality2 x | x < 1     = error "Negative Integer"
             | x == 1    = False
             | otherwise = ldp x == x

-- Sieve of Eratosthenes
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (0 : xs) = sieve xs
sieve (x : xs) = x : sieve (auxSieve xs 1 x)
      where
      auxSieve :: [Integer] -> Integer -> Integer -> [Integer]
      auxSieve [] _ _ = []
      auxSieve (y : ys) a b
          | a == b    = 0 : auxSieve ys 1 b
          | otherwise = y : auxSieve ys (a+1) b
          
primesFromSieve :: [Integer]
primesFromSieve = sieve [2 .. ] 

firstNPrimes :: Int -> [Integer]
firstNPrimes n = take n primesFromSieve

-- remove duplicates from a List
mynub :: (Eq a) => [a] -> [a]
mynub []       = []
mynub [x]      = [x]
mynub (x : xs) = x : mynub (remove x xs)
      where
      remove _ [] = []
      remove y (z : zs) | y == z    = remove y zs
                        | otherwise = z : remove y zs

-- remove the first occurrence of an element from a list
delete :: (Eq a) => a -> [a] -> [a]
delete _ []       = []
delete x (y : ys) = if x == y
                       then ys
                       else y : delete x ys

-- Powerset is a set contaiing all the subsets of the set including an empty set
-- Implementing a Powerlist on the lines of Power Set
powerlist :: [a] -> [[a]]
powerlist [] = [[]]
powerlist (x : xs) = powerlist xs ++ map (x : ) (powerlist xs)

-- λ> powerlist [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- For all n ∈ N, the set of pairs {(a,b) | a,b ∈ N,ab = n,a <= b}
-- is a relation on N. This relation gives all the divisor pairs of n
divisorPairs :: Integer -> [(Integer, Integer)]
divisorPairs n = [(d, quot n d) | d <- [1 .. x], n `rem` d == 0]
             where
             x = floor (sqrt (fromIntegral n))

-- Primality test based on divisorPairs
primality3 :: Integer -> Bool
primality3 n = divisorPairs n == [(1, n)]

--  list of divisors of a natural number, the list of all proper divisors 
--  of a natural number, and a test for being a perfect natural number
divs :: Integer -> [Integer]
divs n = L.sort $ L.foldl' (\acc (a, b) -> a : b : acc) [] (divisorPairs n)

properDivs :: Integer -> [Integer]
properDivs n = init (divs n)

isPerfect :: Integer -> Bool
isPerfect n = sum (properDivs n) == n

-- Modulo
modulo :: Integer -> Integer -> Integer -> Bool
modulo a b = divides (a - b)
-- λ> filter (modulo 10 7) [-10 .. 10]
-- [-9,-6,-3,0,3,6,9]

-- Integer Partitions of n ∈ N+ are lists of non-zero natural
-- numbers which all add upto the number n exactly
{--
Algorithm for generating the integer partitions in lexicographically
increasing order. The integer partitions of n correspond to the sizes 
of the set partitions of a set A with |A| = n.

The first integer partition of n is [n].
Let B be the last integer partition generated. If B consists of only 1’s, 
then done. Otherwise, there is a smallest non-1 part m. To generate the 
next partition, subtract 1 from m and collect all the units so as to 
match the new smallest part m − 1.
An integer partition is represented as a list of integers. 
For convenience we count the number of 1’s, and then remove them from the 
partition. This gives us a compressed representation like
(2, [3, 3]) of [1, 1, 3, 3]. 
These compressed partitions have type ComPartition.
-}
type Partition    = [Int]
type ComPartition = (Int, Partition)

-- Expansion of a compressed partition (n, p) is done by generating n 1’s followed by p
expand :: ComPartition -> Partition
expand (0, p) = p
expand (n, p) = 1 : expand (n - 1, p)

nextPartition :: ComPartition -> ComPartition
nextPartition (a, x : xs) = pack (x - 1) (a + x, xs)

pack :: Int -> ComPartition -> ComPartition
pack 1 (b, xs) = (b, xs)
pack a (b, xs) = if a > b
                    then pack (a - 1) (b, xs)
                    else pack a (b - a, a : xs)

generatePartitions :: ComPartition -> [Partition]
generatePartitions (n, []) = [expand (n, [])]
generatePartitions (n, x : xs) = expand (n, x : xs) : generatePartitions (nextPartition (n, x : xs))

partition :: Int -> [Partition]
partition n
    | n < 1     = error "Negative Argument"
    | n == 1    = [[1]]
    | otherwise = generatePartitions (0, [n])
