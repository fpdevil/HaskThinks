{-# LANGUAGE FlexibleContexts #-}
module RandomMath where

import qualified Data.List as L
import qualified Data.Char as C

import Prelude hiding (gcd, lcm, Rational)

-- Represent Greek symbols
data Greek = Alpha | Beta | Gamma | Delta | Epsilon | Zeta
            | Eta | Theta | Iota | Kappa | Lambda | Mu | Nu
            | Xi | Omicron | Pi | Rho | Sigmaf | Sigma | Tau 
            | Upsilon | Phi | Chi | Psi | Omega | Thetasym 
            | Upsih | Piv
            deriving (Eq, Ord, Enum)

-- Making instance of Show
instance Show Greek where
    show Alpha    = "\ESC[231;31m\945 \ESC[0m "
    show Beta     = "\ESC[231;31m\946 \ESC[0m "
    show Gamma    = "\ESC[231;31m\947 \ESC[0m "
    show Delta    = "\ESC[231;31m\948 \ESC[0m "
    show Epsilon  = "\ESC[231;31m\949 \ESC[0m "
    show Zeta     = "\ESC[231;31m\950 \ESC[0m "
    show Eta      = "\ESC[231;31m\951 \ESC[0m "
    show Theta    = "\ESC[231;31m\952 \ESC[0m "
    show Iota     = "\ESC[231;31m\953 \ESC[0m "
    show Kappa    = "\ESC[231;31m\954 \ESC[0m "
    show Lambda   = "\ESC[231;31m\955 \ESC[0m "
    show Mu       = "\ESC[231;31m\956 \ESC[0m "
    show Nu       = "\ESC[231;31m\957 \ESC[0m "
    show Xi       = "\ESC[231;31m\958 \ESC[0m "
    show Omicron  = "\ESC[231;31m\959 \ESC[0m "
    show Pi       = "\ESC[231;31m\960 \ESC[0m "
    show Rho      = "\ESC[231;31m\961 \ESC[0m "
    show Sigmaf   = "\ESC[231;31m\962 \ESC[0m "
    show Sigma    = "\ESC[231;31m\963 \ESC[0m "
    show Tau      = "\ESC[231;31m\964 \ESC[0m "
    show Upsilon  = "\ESC[231;31m\965 \ESC[0m "
    show Phi      = "\ESC[231;31m\966 \ESC[0m "
    show Chi      = "\ESC[231;31m\967 \ESC[0m "
    show Psi      = "\ESC[231;31m\968 \ESC[0m "
    show Omega    = "\ESC[231;31m\969 \ESC[0m "
    show Thetasym = "\ESC[231;31m\977 \ESC[0m "
    show Upsih    = "\ESC[231;31m\978 \ESC[0m "
    show Piv      = "\ESC[231;31m\982 \ESC[0m "


-- Image and CoImage of functions
{--
if f ：X → Y , A ⊆ X and Β ⊆ Y

1. f[A] = {f(x) | x ∈ A} is the image of Α under f
2. f¯¹[B] = {x ∈ X | f(x) ∈ B} is the co-image of Β under f

λ> image (* 2) [1,2,3]
[2,4,6]
λ>  coImage (* 2) [1, 2, 3] [2,3,4]
[1,2]
-}
image :: (Eq b) => (a -> b) -> [a] -> [b]
image f xs = L.nub [f x | x <- xs]

coImage :: (Eq b) => (a -> b) -> [a] -> [b] -> [a]
coImage f xs ys = [x | x <- xs, f x `elem` ys]

-- Below 2 Q from https://programmingpraxis.com/2015/12/18/two-part-interview-question/2/
-- First non-repeating element of a List
firstNonRepeat :: (Eq a) => [a] -> Maybe a
firstNonRepeat [] = Nothing
firstNonRepeat [x] = Just x
firstNonRepeat (x : xs) = if x `notElem` tail xs
                             then Just x
                             else firstNonRepeat xs

-- first element that appears an even number of times in an unsorted array
firstEvenRepeat :: (Eq a) => [a] -> Maybe a
firstEvenRepeat [] = Nothing
firstEvenRepeat xs = if even (length $ fst p)
                     then Just (head $ fst p)
                     else firstEvenRepeat (snd p)
                     where
                     p = L.partition (== head xs) xs

-- main = do
--     return $ firstEvenRepeat [5,3,5,1,5,1,3]

{--
INTEGER PARTITIONS of n ∈ N+
Implementing a recursive function for defining the partition of a number,
p(n) for any +ve integer.

eg. p(4) = 4
           3 + 1
           2 + 2
           2 + 1 + 1
           1 + 1 + 1 + 1

Algorithm:
In order to define partition of a number, p(n) we need to define an auxilliary
function p(n, x) which would represent the partitions of n which only use 
numbers ≥ x in the sum. For example in the above p(4, 2) = 2 since the only 2
partitions of 4 given by 4 and 2 + 2 involve involve numbers ≥ 2.

The point of introducing p(x, n) is that it can be defined recursively using
a set of base cases for recusrsion.
p(n, x) = number of partitions of n with x parts
1. p(n, x) = 0 if x ≤ 0 or n ≤ 0
2. p(n, x) = 0 if x ≻ n
3. p(n, x) = 1 if x ≈ n
4. p(n, x) = p(n, x + 1) + p(n - x, x) otherwise

In this case our original p(n) is equal to p(n, 1), representing all
partitions made up of integers ≥ 1.

-- made using xah-math-input-mode toggling
-}

-- Auxilliary function for getting the number of partitions of a number
partitions :: Int -> Int
partitions n = p (n, 1)

-- number of partitions of n with x parts (largest part is x)
p :: (Int, Int) -> Int
p (n, x)
   | n <= 0 || x <= 0 = 0
   | x > n = 0
   | n == x = 1
   | otherwise = p (n, x + 1) + p (n - x, x)

{--
Α function f ：X → Ψ is called
Surjective Functions (Onto)
    - if every element b ∈ Y occurs as a function value of
      atleast one a ∈ X ie., f(X) = Y
      Means that every b has at least one matching a (maybe more than one)
      There won't be a b left out. (One-To-Many is OK)
      
Injective Functions (One-To-One)
    - if every b ∈ Y is a value of at the most one a ∈ X
      Means that every member of a has its own unique matching member in b
      (One-To-Many is NOΤ OK)
      f(x) = f(y) =⇒ x=y.
    
Bijective Function (Bijection)
    - if function is both Surjective ＆ Injective
      Both One-One and Onto
-}
-- If the domain of a function is represented as a list, 
-- injective test can be implemented as follows
isInjective :: (Eq b) => (a -> b) -> [a] -> Bool
isInjective _ []       = True
isInjective f (x : xs) = f x `notElem` image f xs && isInjective f xs

-- if the domain and co-domain of a function are represented as lists
-- surjective test can be implemented as follows
isSurjective :: (Eq b) => (a -> b) -> [a] -> [b] -> Bool
isSurjective _ _ []        = True
isSurjective f xs (y : ys) = 
             y `elem` image f xs && isSurjective f xs ys
             
-- isBijective
isBijective :: (Eq b) => (a -> b) -> [a] -> [b] -> Bool
isBijective f xs ys = isInjective f xs && isSurjective f xs ys

-- A Simple BinaryTree
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Create a leaf
leaf :: a -> Tree a
leaf x = Node x Leaf Leaf

-- Create a Tree
mkTree :: (Eq a, Num a) => a -> Tree a
mkTree 0 = Leaf
mkTree n = Node n (mkTree (n-1)) (mkTree (n-1))

-- indenting for pretty printing the tree
indent :: (Show a) => Tree a -> [String]
indent Leaf = ["---"]
indent (Node c l r) = ["--" ++ show c] ++ fmap ("  |" ++) li ++ ("  !" ++ ri) : fmap ("  " ++) rj
       where
       (ri : rj) = indent r
       li = indent l

prettyTree :: (Show a) => Tree a -> IO ()
prettyTree t = mapM_ putStrLn $ indent t

-- number of elements of the tree
count :: Num a => Tree b -> a
count Leaf = 1
count (Node _ l r) = count l + count r + 1

-- count should be 2ⁿ-1
-- λ> count (mkTree 6) == 2^7 - 1
-- True

-- depth of Tree
depth :: (Num a, Ord a) => Tree b -> a
depth Leaf = 0
depth (Node _ l r) = max (depth l) (depth r) + 1

-- check if tree is balanced
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l r) = isBalanced l
                       && isBalanced r
                       && depth l == depth r

{--
Conversion functions
Conversion of a tree into a list can be done in various ways, depending on when the node is visited

1. Preorder traversal of a tree is the result of first visiting the node, next visiting the 
   left subtree, and finally visiting the right subtree.
2. Inorder traversal of a tree is the result of first visiting the left subtree, next visiting 
   the node, and finally visiting the right subtree.
3. Postorder traversal of a tree is the result of first visiting the left subtree, next visiting 
   the right subtree, and finally visiting the node.
-}

-- Tree to List
tree2list :: Tree a -> [a]
tree2list Leaf = []
tree2list (Node c l r) = [c] ++ tree2list l ++ tree2list r

-- List to Tree
list2tree :: (Ord a) => [a] -> Tree a
list2tree [] = Leaf
list2tree [c] = Node c Leaf Leaf
list2tree (c : cs) = Node c (list2tree lsubt) (list2tree rsubt)
          where
          (lsubt, rsubt) = L.span (<= c) cs

-- Min and Max Values of a Tree
-- Min value will be in the left Node
-- Max value will be in the right Node
minVal :: Tree a -> Maybe a
minVal Leaf = Nothing
minVal (Node c Leaf _) = Just c
minVal (Node _ l _) = minVal l

maxVal :: Tree a -> Maybe a
maxVal Leaf = Nothing
maxVal (Node c _ Leaf) = Just c
maxVal (Node _ _ r) = maxVal r

-- Inverting the Binary Tree
invert :: Tree a -> Tree a
invert Leaf = Leaf
invert (Node c l r) = Node c (invert r) (invert l)

-- check if an element is in Tree
isElem :: (Ord a) => Tree a -> a -> Bool
isElem Leaf _ = False
isElem (Node c l r) e = case e `compare` c of
                          EQ -> True
                          LT -> isElem l e
                          GT -> isElem r e
                          
-- insert an element into a Tree
insertElem :: (Ord a) => Tree a -> a -> Tree a
insertElem Leaf e = leaf e
insertElem (Node c l r) e = case e `compare` c of
                              EQ -> Node c l r
                              LT -> Node c (insertElem l e) r
                              GT -> Node c l (insertElem r e)

-- Binary string representation of an integral number
binary :: (Integral a) => a -> [a]
binary n = reverse $ bits n
       where
       bits x
           | x == 0    = [0]
           | x == 1    = [1]
           | otherwise = b : bits a
            where
            (a, b) = x `divMod` 2

-- To display the binary representation on the screen
-- an auxilliary function intToDigit for converting
-- Integer to Character is defined
showDigits :: [Int] -> String
showDigits = map C.intToDigit

bin :: Int -> String
bin = showDigits . binary

-- Euler's gcd
gcd :: (Integral a) => a -> a -> a
gcd 0 0 = error "RandomMath.gcd: gcd 0 0 is undefined"
gcd a b = gcdx (abs a) (abs b)
    where
    gcdx x 0 = x
    gcdx x y = gcdx y (x `rem` y)
    
-- Least common multiple lcm
lcm :: (Integral a) => a -> a -> a
lcm 0 _ = 0
lcm _ 0 = 0
lcm a b = abs ((a `quot` gcd a b) * b)

-- Implementation of Rational Arithmetic
data Ratio a = a :% a deriving (Eq)
type Rational = Ratio Integer

(%) :: (Integral a) => a -> a -> Ratio a
x % y = reduce (x * signum y) (abs y)

reduce :: (Integral a) => a -> a -> Ratio a
reduce a b
    | b == 0    = error "RandomMath.%: zero denominator"
    | otherwise = (a `quot` d) :% (b `quot` d)
    where d = gcd a b

-- Functiona for extracting numerator and denominator
numerator, denominator :: (Integral a) => Ratio a -> a
numerator (x :% _)   = x
denominator (_ :% y) = y 

instance Integral a => Ord (Ratio a) where
         compare (x :% y) (x' :% y') = compare (x*y') (x'*y)
         
instance Integral a => Num (Ratio a) where
         (x :% y) + (x' :% y') = reduce (x*y' + y*x') (y*y')
         (x :% y) - (x' :% y') = reduce (x*y' - y*x') (y*y')
         (x :% y) * (x' :% y') = reduce (x*x') (y*y')
         negate (x :% y)       = negate x :% y
         abs (x :% y)          = abs x :% y
         signum (x :% _)       = signum x :% 1
         fromInteger x         = fromInteger x :% 1

instance Integral a => Fractional (Ratio a) where
         (x :% y) / (x' :% y') = (x*y') :% (y*x')
         recip (0 :% _)          = error "Zero error"
         recip (x :% y)          = if x < 0 then (-y) :% (-x) else y :% x
         --fromRational (x :% y) = (fromInteger x) :% (fromInteger y)

-- Decimal expansion of a Fractional
decExpn :: Rational -> [Integer]
decExpn x
    | x < 0     = error "negative argument"
    | r == 0    = [q]
    | otherwise = q : decExpn ((r * 10) % d)
    where
    (q, r) = quotRem n d
    n = numerator x
    d = denominator x

-- λ> take 20 $ decExpn (1 % 7)
-- [0,1,4,2,8,5,7,1,4,2,8,5,7,1,4,2,8,5,7,1]

-- Difference sequences
-- d(f) = λn.a𝔫₊₁ - a𝔫
difs :: [Integer] -> [Integer]
difs [] = []
difs [_] = []
difs (x : y : zs) = (y - x) : difs (y : zs)

-- if a given input list has a polynomial form of degree k, 
-- then after k steps of taking differences the list is reduced to a constant list
--
-- For a Polynomial function, f = λn.(2n² + n + 1) a second degree polynomial
-- λ> take 15 $ map (\n -> (2*n^2 + n + 1)) [0..]
-- [1,4,11,22,37,56,79,106,137,172,211,254,301,352,407]
--
-- λ> difs [1,4,11,22,37,56,79,106,137,172,211,254,301]
-- [3,7,11,15,19,23,27,31,35,39,43,47]
-- λ> difs $ difs [1,4,11,22,37,56,79,106,137,172,211,254,301]
-- [4,4,4,4,4,4,4,4,4,4,4]
-- The following function keeps generating difference lists until the differences get constant.
difLists :: [[Integer]] -> [[Integer]]
difLists [] = []
difLists lists@(xs : xss) =
    if constant xs
       then lists
       else difLists (difs xs : lists)
       where
       constant (a : b : bs) = all (== a) (b : bs)
       constant _            = error "Not a Polynomial or lack of data"

{-
λ> difLists [[1,4,11,22,37,56,79,106,137,172,211,254,301]]
   [[4,4,4,4,4,4,4,4,4,4,4],
    [3,7,11,15,19,23,27,31,35,39,43,47],
    [1,4,11,22,37,56,79,106,137,172,211,254,301]]
    
The list of differences can be used to generate the next element of the 
original sequence. If we add the last elements of all the difference lists 
to the last element of the original sequence, we will get the next element
of the original list.

The next element of the original list [1,4,11,22,37,56,79,106,137,172,211,254,301,352]
after 301 is 352, which is
4 + 47 + 301 (last element of each individual list)
--}

-- A Function for getting the last element list of the difLists
genDifs :: [Integer] -> [Integer]
genDifs xs = map last $ difLists [xs]

-- λ> genDifs [1,4,11,22,37,56,79,106,137,172,211,254,301]
-- [4,47,301]

{--
A new list of last elements of difference lists is computed 
from the current one by keeping the constant element d₁ and 
replacing each dᵢ₊₁ by dᵢ + dᵢ₊₁
-}
nextD :: [Integer] -> [Integer]
nextD []           = []
nextD [x]          = [x]
nextD (x : y : zs) = x : nextD (x + y : zs)

-- The next element of the original sequence is given by the
-- last element of the new list of last elements of difference lists
-- λ> next [1,4,11,22,37,56,79,106,137,172,211,254,301]
-- 352
next :: [Integer] -> Integer
next = last . nextD .genDifs

{--
For generating the series of next elements of the polynomial

λ> take 20 $ map (\n -> (2*n^2 + n + 1)) [0..]
[1,4,11,22,37,56,79,106,137,172,211,254,301,352,407,466,529,596,667,742]

λ> take 10 $ continue [1,4,11,22,37,56,79,106,137,172]
[211,254,301,352,407,466,529,596,667,742]
-}
continue :: [Integer] -> [Integer]
continue xs = map last $ iterate nextD differences
         where
         differences = nextD $ genDifs xs

-- Α list of sums of squares
-- ∑(∑n²) = 1² + (1² + 2²) + (1² + 2² + 3²) + ...
--        = 1 + 5 + 14 + 30 + 55...
-- λ> take 10 (continue [1, 5, 14, 30, 55])
-- [91,140,204,285,385,506,650,819,1015,1240]

-- If a given list is generated by a polynomial, then the 
-- degree of the polynomial can be computed by difference analysis
degree :: [[Integer]] -> Int
degree xs = length (difLists xs) - 1
