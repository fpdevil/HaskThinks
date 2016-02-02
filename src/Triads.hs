-- Pythagorean triplets or triads
-- a, b, c are triples of numbers such that a^2 + b^2 = c^2
-- 1 <= a,b,c < n for some given n
-- additional restrictions are
-- a and b are coprime (no common factor) & a < b
-- 2a^2 < a^2+b^2 = c^2 <= n^2
-- i.e., a <= floor(n/sqrt(2))

triads :: Int -> [(Int, Int, Int)]
triads n = [(a,b,c) | a <- [1 .. m], b <- [a+1 .. n],
                            coprime a b,
                            c <- [b+1 .. n],
                            a*a + b*b == c*c]
          where
          m = floor (fromIntegral n / sqrt 2)

divisors :: Int -> [Int]
divisors x = [d | d <- [2 .. x-1], x `mod` d == 0]

coprime :: Int -> Int -> Bool
coprime a b = disjoint (divisors a) (divisors b)

disjoint :: [Int] -> [Int] -> Bool
disjoint xs [] = True
disjoint [] ys = True
disjoint (x:xs) (y:ys)
         | x < y = disjoint xs (y:ys)
         | x == y = False
         | x > y = disjoint (x:xs) ys

-- Merge Sort
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort ys) (sort zs)
          where
          (ys, zs) = halve xs

halve :: (Ord a) => [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
           where
           n = length xs `div` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
      | x < y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Ramanujan's Taxicab quadruple
-- a^3 + b^3 = c^3 + d^3
-- (a, b, c, d) | a <= b , c <= d, a < c
taxicab n = [(a, b, c, d) | a <- [1 .. n],
                            b <- [a .. n],
                            c <- [a+1 .. n],
                            d <- [c .. n],
                            a^3 + b^3 == c^3 + d^3]
