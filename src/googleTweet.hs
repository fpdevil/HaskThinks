-- Decode google tweet
-- Google's first tweet
-- I’m 01100110 01100101 01100101 01101100 01101001 01101110 01100111 00100000 01101100 01110101 01100011 01101011 01111001 00001010

import qualified Data.Char as C

pos :: String -> [(Int, Char)]
pos [] = []
pos [x] = [(0,x)]
pos (x:xs) = (length xs, x) : pos xs

tweet :: String
tweet = "01100110 01100101 01100101 01101100 01101001 01101110 01100111 00100000 01101100 01110101 01100011 01101011 01111001 00001010"

posList :: String -> [[(Int, Char)]]
posList = map pos . words

tweetList :: String -> [[(Int, Char)]]
tweetList = posList

binList :: [(Int, Char)] -> Int
binList l = sum [C.digitToInt b * (2 ^ a) | (a, b) <- l]

decodeList :: String -> String
decodeList = map (C.chr . binList) . tweetList

main :: IO ()
main = putStr $ "I'am " ++ decodeList tweet
