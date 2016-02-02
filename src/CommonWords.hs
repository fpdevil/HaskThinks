-- Get n most Common Words in a text
-- As an example get 50 most common words from PrideAndPrejudice
-- output running from repl
-- λ> :main
-- Read text from where:
-- PrideAndPrejudice.txt
-- How many Common words:
-- 50
-- Place results in which file:
-- output.txt
-- writing cwords done!
--
module CommonWords  where

import           Data.Char   (toLower)
import           Data.List   (group, sort, sortBy)
import           Debug.Trace
import           System.IO

-- for debugging purpose
debug :: c -> String -> c
debug = flip trace

type Words = String
type Code = (Int, Words)

sortWords :: [Words] -> [Words]
sortWords = sort

-- list of tuples containing the number of occurrences
-- of each word in the sentence
countRuns :: [Words] -> [Code]
countRuns [] = []
countRuns (w : ws) = (1 + length a, w) : countRuns b -- `debug` "  a, w  "
         where
                (a, b) = span (== w) ws -- `debug`  "a, b"

-- second version of count runs using map
countRuns' :: [Words] -> [Code]
countRuns' [] = []
countRuns' w = map (\x -> (length x, head x)) $ group w

sortCodes :: [Code] -> [Code]
sortCodes = sortBy (flip compare)

-- for display purpose
showRun :: Code -> String
showRun (n, w) = w ++ ":" ++ show n ++ "\n"

-- first n common words from a sentence or a paragraph
commonWords :: Int -> String -> String
commonWords n = concatMap showRun . take n . sortCodes . countRuns' . sortWords . words . map toLower

-- read from a file for grabbing the text input
cwords :: Int -> FilePath -> FilePath -> IO ()
cwords n infile outfile = do
   text <- readFile infile
   writeFile outfile (commonWords n text)
   putStrLn "cwords written to output file!"

main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   putStrLn "Read text from where: "
   infile <- getLine
   putStrLn "How many Common words: "
   n <- getLine
   putStrLn "Place results in which file: "
   outfile <- getLine
   text <- readFile infile
   writeFile outfile (commonWords (read n) text)
   putStrLn "writing cwords done!"
