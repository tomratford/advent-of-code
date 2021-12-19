{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (nub, isPrefixOf, (\\), elemIndices, sortBy)
import Data.Map ((!))
import qualified Data.Map as M

--Type decs
type Rules = M.Map String Char
--type Bigram = M.Map String (Integer,[String])
type Sums = M.Map Char Integer

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
  let --RULES
      newCrudeRules = map getRules' (drop 2 fileLines) --rule as list make into map
      newRules = M.fromList newCrudeRules --Map version of rules
      initPairs = getInitPairs template --List of init rules
      --BIGRAM
      allStrs = map fst $ M.toList rules 
      initBigram = foldl incMap (M.fromList $ zip allStrs (repeat 0)) initPairs
      stepsNew = iterate (incBigram newRules) initBigram
      --SUMS
      allChars = map snd $ M.toList rules --List of all chars for sums
      basicSums = M.fromList $ zip allChars (repeat 0)
      initSums = addToSums (addToSums basicSums (head template,1)) (last template, 1)
  print $ stepsNew !! 2
  print $ stepsNew !! 3
  print $ stepsNew !! 4
  --print $ bigramToSums (stepsNew !! 10) initSums

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
getRules' :: String -> (String, [String])
getRules' line = (str,[[head str,chr],[chr,last str]])
  where ws = words line
        str = head ws
        chr = (head . last) ws

getInitPairs :: String -> [String]
getInitPairs [x,y] = [x:[y]]
getInitPairs (x:y:xs) = (x:[y]) : getInitPairs (y:xs)

--Increment/loop etc.
incMap :: Bigram -> String -> Bigram
incMap bigram pair = M.adjust (+1) pair bigram

decMap :: Bigram -> Bigram
decMap = M.map (\ x -> if x > 0 then x - 1 else 0)

incBigram :: BiRules -> Bigram -> Bigram
incBigram rules bigram = foldl incMap bigram incList
  where gt0List = M.keys $ M.filter (>0) bigram
        incList = concat $ M.elems $ M.filterWithKey (\ k _ -> k `elem` gt0List) rules

--Add to sums for final output
addToSums :: Sums -> (Char,Integer) -> Sums
addToSums sums (char,int) = M.adjust (int +) char sums

bigramToSums :: Bigram -> Sums -> Sums
bigramToSums bigram sums = foldl addToSums sums list
  where crudeList = M.toList bigram
        list = concatMap (\x@(str,int) -> [(head str, int), (last str, int)]) crudeList

--Part 2 rewrite
type BiRules = M.Map String [String]
type Bigram = M.Map String Integer
