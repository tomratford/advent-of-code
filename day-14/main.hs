{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (nub, isPrefixOf, (\\), elemIndices, sortBy)
import Data.Map ((!))
import qualified Data.Map as M

--Type decs
type Rules = M.Map String Char
type Bigram = M.Map String (Integer,[String])

main = do
  putStrLn "File:"
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile
      template = head fileLines
      crudeRules = map getRules (drop 2 fileLines)
      rules = M.fromList crudeRules
      steps = iterate (loopStr rules) template
  --print $ part1 (steps !! 10) -- Lanternfish grrr. Commented for performance
  let newCrudeRules = map getRules' (drop 2 fileLines)
      newRules = M.fromList newCrudeRules
      initPairs = getInitPairs template
      initRules = foldl incMap newRules initPairs
  print $ newRules
  print $ initPairs
  print $ initRules

--Parse rules
getRules :: String -> (String,Char)
getRules line = (str,chr)
  where ws = words line
        str = head ws
        chr = (head . last) ws

--One loop over str
loopStr :: Rules -> String -> String
loopStr rules [x, y] = [x, e, y]
  where e = rules ! [x, y]
loopStr rules (x:y:xs) = x : e : loopStr rules (y:xs)
  where e = rules ! [x, y]

--Get part 1 answer
part1 :: String -> Int
part1 str = (snd . head) sorted - (snd . last) sorted
  where mostCommon = nub [(elem, count) | elem <- str, let count = length (filter (==elem) str)]
        sorted = sortBy (\x y -> compare (snd y) (snd x) ) mostCommon

--Pt2

--Parse rules to a map
getRules' :: String -> (String, (Integer,[String]))
getRules' line = (str,(0,[[head str,chr],[chr,last str]]))
  where ws = words line
        str = head ws
        chr = (head . last) ws

getInitPairs :: String -> [String]
getInitPairs [x,y] = [x:[y]]
getInitPairs (x:y:xs) = (x:[y]) : getInitPairs (y:xs)

incMap :: Bigram -> String -> Bigram
incMap bigram pair = M.adjust (\x -> (fst x * 2, snd x)) pair bigram

incBigram :: Bigram -> Bigram
incBigram bigram = 
  where get