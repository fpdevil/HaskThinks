-- A Simple Sudoku solver
module Sudoku where

-- Type specifications for the Matrix (9X9)
-- define basic data types for a Matrix and Rows

type Row a = [a]
type Matrix a = [Row a]

-- define a grid of 9 X 9 Matrix of digits
type Digit = Char
type Grid = Matrix Digit

-- valid digits are from 1 through 9 (0 is blank)
digits :: String
digits = ['1' .. '9']

-- blank for 0
blank :: Digit -> Bool
blank = (== '0')


-- specification 1
-- start with the given grid and fill it with all the possible
-- values for blank entries. Result will be a list of grids filled.
-- Then filter this list for those which do not contain duplicates
-- in any Row, Column or Box.
solver1 :: Grid -> [Grid]
solver1 = filter validSets . allCombinations

-- Auxilliary function definitions
allCombinations :: Grid -> [Grid]
allCombinations = expandAll . allChoices

-- function allChoices installs the available digits into each cell
-- it will go through the Grid (a list of list) and map a function on
-- each elements of the internal lists. After applying allChoices we
-- obtain a matrix each of whose entries is a list of digits.
allChoices :: Grid -> Matrix [Digit]
allChoices = map (map choice)

-- aux function choice to install the available digits to each cell
-- if a cell is blank all digits are installed as a possible choice
-- otherwise there is only one choice and a singleton returned
choice :: Digit -> [Digit]
choice d = if blank d
              then digits
              else [d]


-- After applying allChoices we obtain a matrix each of whose entries is
-- a list of digits. What we want to do next is to define expandAll to
-- convert this matrix into a list of grids by installing all the choices
-- in all possible ways. This is a cartesiian product of list(list)
-- example convert [[1,2,3],[2],[1,3]] to
-- [[1,2,1],[1,2,3],[2,2,1],[2,2,3],[3,2,1],[3,2,3]]
cartProd :: [[a]] -> [[a]]
cartProd [] = [[]]
cartProd (xs : xss) = [x:y | x <- xs, y <- ys]
         where
         ys = cartProd xss

-- for installing all possible choices in all ways
-- returns empty list if any element in any row is an empty list.
expandAll :: Matrix [Digit] -> [Grid]
expandAll = cartProd . map cartProd

-- identify duplicates in a list
nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = x `notElem` xs && nodups xs

-- get rows of the grid
-- If a matrix is given by a list of its rows, then rows is just the identity function on matrices
rows :: Matrix a -> Matrix a
rows = id

-- cols of the grid, which is nothing but rows of transpose of matrix
cols :: Matrix a -> Matrix a
cols [] = []
cols [xs] = [[x] | x <- xs]
cols (x : xs) = zipWith (:) x (cols xs)

-- group n elements in a list together as a sublist
grp :: Int -> [a] -> [[a]]
grp _ [] = []
grp n xs = take n xs : grp n (drop n xs)

-- group 3 elements
group :: [a] -> [[a]]
group = grp 3

-- ungroup the elements
ungroup :: [[a]] -> [a]
ungroup = concat

-- get boxs of size 3 from the Matrix
boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

-- a valid grid is the one which does not have duplicates
-- in any row, column or box
validSets :: Grid -> Bool
validSets grid = all nodups (rows grid) &&
                 all nodups (cols grid) &&
                 all nodups (boxs grid)

