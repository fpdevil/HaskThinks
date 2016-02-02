{--
The credit card company Foxy issues cards with ten-digit card-identification numbers (CINs).
The first eight digits are arbitrary but the number formed from the last two digits is a
checksum equal to the sum of the first eight digits.
For example, "6324513428" is a valid CIN because the sum of the first eight digits is 28.
Construct a function addSum :: CIN -> CIN that takes a string consisting of eight digits and
returns a string of ten digits that includes the checksum. Thus CIN is a type synonym for String,
though restricted to strings of digits.
--}
module Foxy where

import qualified Data.Char as C

type CIN = [Char]

-- get digits from the CIN
getDigit :: Char -> Int
getDigit c = read [c]

addSum :: CIN -> CIN
addSum cin =
       cin ++ show (n `div` 10) ++ show (n `mod` 10)
       where
       n = sum $ map getDigit cin

valid :: CIN -> Bool
valid cin = cin == addSum (take 8 cin)

-- palindrome
palindrome :: IO ()
palindrome =
           do {putStrLn "Enter a String: ";
               xs <- getLine;
               if isPalindrome xs
                  then putStrLn "YES"
                  else putStrLn "NO"}

isPalindrome :: String -> Bool
isPalindrome xs = ys == reverse ys
             where ys = map C.toLower (filter C.isAlpha xs)

