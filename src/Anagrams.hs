{--
For finding the Anagrams of length n

Running the program
Without compiling
runhaskell Anagrams.hs 6 < dictionary

Compiled Version
This invokes the Glasgow Haskell Compiler (ghc)
and creates an executable file anagrams

To compile this program, type
$ ghc -o anagrams Anagrams.hs
------------------------------
Anagrams.hs
Anagrams.hi
Anagrams.o
anagrams
------------------------------

$ ./anagrams 6 < dictionary

This reads the file dictionary and prints the 6-
letter anagrams to the terminal. If you want the
result to go to an output file called "results",
you can type
Example for 24 character words
$ ./anagrams 24 < dictionary > results

Output from results file
Getting Anagrams in Combination : Words format
anagrams
----------------------------------------------
acccefhhiiiiillnoooppsst: scientificophilosophical
aadeeehhhiillnnooopprttt: tetraiodophenolphthalein
aaddeeefhhlllmooprstuxyy: formaldehydesulphoxylate
aacdeehhiimoooprrrtttyyz: thyroparathyroidectomize
aacccgghhiillloooooppsty: pathologicopsychological
-}

import           Data.List          (sort)
import           System.Environment (getArgs, getProgName)

-- type specifications
type Words = String
type Label = String

-- Get all the words of a specified length
getWords :: Int -> [Words] -> [Words]
getWords n = filter (\x -> length x == n)

-- Take each word and add a label to it. The label consists of the characters
-- of the word, sorted into alphabetical order.
addLabel :: Words -> (Label, Words)
addLabel w = (sort w, w)

-- Sort the list of labelled words into alphabetical order of label
sortLabels :: [(Label, Words)] -> [(Label, Words)]
sortLabels = sort

-- Replace each group of adjacently labelled words with the same label with a
-- single entry consisting of a pair in which the first component is the common
-- label and the second component is a list of words with that label
groupByLabel :: [(Label, Words)] -> [(Label, [Words])]
groupByLabel = foldr auxGroup []

-- Helper for parsing the input to be used in the grooupByLabel function
auxGroup :: (Label, Words) -> [(Label, [Words])] -> [(Label, [Words])]
auxGroup (x, a) [] = [(x, [a])]
auxGroup (x, a) ((y, b) : ybs)
    | x == y        = (x, a : b) : ybs
    | otherwise     = (x, [a]) : (y, b) : ybs

-- Helper function for formatted display
auxDisplay :: [Words] -> String
auxDisplay []       = []
auxDisplay [w]      = w
auxDisplay (w : ws) = w ++ ", " ++ auxDisplay ws

-- Replace each entry by a string using a function
-- and then concatenate the results
showEntry :: [(Label,[Words])] -> String
showEntry lws = concat $ foldl (\acc (l, ws) -> (l ++ ": " ++ auxDisplay ws ++ "\n") : acc) [] lws

-- Final Anagram result
anagram :: Int -> [Words] -> String
anagram n = showEntry . groupByLabel . sortLabels . map addLabel . getWords n

main :: IO ()
main = do
    putStrLn "Getting Anagrams in Combination : Words format"
    [num]    <- getArgs
    text     <- getContents
    progName <- getProgName
    putStrLn progName
    putStrLn "----------------------------------------------"
    putStr (anagram (read num) (words text))
