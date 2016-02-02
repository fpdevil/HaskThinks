-- Numbers to Words
{--
λ> main
Enter a number to represent as string
345678
"three hundred and forty-five thousand six hundred and seventy-eight"
--}
module Num2Words where

-- Build Word lists for numbers
units :: [String]
units = ["zero", "one", "two", "three", "four",
         "five", "six", "seven", "eight", "nine"]

tens :: [String]
tens = ["twenty", "thirty", "forty", "fifty",
        "sixty", "seventy", "eighty", "ninety"]

teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen",
         "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

divmod :: Int -> Int -> (Int, Int)
divmod a b = a `divMod` b

-- converting 1 digit numbers 0 to 9
convert1 :: Int -> String
convert1 n = units !! n

-- converting 2 digit numbers from 0 till 99 (0 <= n < 100)
-- first split the number into digits using auxilliary function divmod
-- representing digits into strings by splitting number with 10
convert2 :: Int -> String
convert2 n
    | t == 0 = units !! u
    | t == 1 = teens !! u
    | t >= 2 && u == 0 = tens !! (t - 2)
    | otherwise = tens !! (t - 2) ++ "-" ++ units !! u
      where
      (t, u) = divmod n 10

-- converting 3 digit numbers from 0 till 999 (0 <= n < 1000)
-- first split the number into digits to represent
-- 100's and tens of the positions of the Number
-- This representation is done by splitting with 100
convert3 :: Int -> String
convert3 n
    | h == 0 = convert2 t
    | t == 0 = units !! h ++ " hundred"
    | otherwise = units !! h ++ " hundred and " ++ convert2 t
      where
      (h, t) = divmod n 100

-- converting higher numbers of upto 6 digits from 0 till 999999
-- (0 <= n < 1000000)
-- Split the number into million's and 1000's places
-- This has to be done by splitting with 1000
convert6 :: Int -> String
convert6 n
    | m == 0 = convert3 th
    | th == 0 = convert3 m ++ " thousand"
    | otherwise = convert3 m ++ " thousand " ++ apply th ++ convert3 th
      where
      (m, th) = divmod n 1000

apply :: Int -> String
apply n = if n < 100
             then "and "
             else ""

main :: IO ()
main = do
    putStrLn "Enter a number to represent as string"
    num <- getLine
    print  $ convert6 (read num)
