import Data.List ((\\), group, sort, isPrefixOf, intersect, elemIndices)
import Debug.Trace (trace)

main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  let ls = lines rawFile
      corrupts = map (loopParse) ls
      earliest = map (earliestCorrupt) (filter (not . null . (intersect ")}>]")) corrupts)
      val      = map simpVal earliest
      --pt2
      noCorrupts = filter (null . (intersect ")}>]")) corrupts
      noCorruptsScore = map (foldr (\x y -> y*5 + (simpVal2 x)) 0) noCorrupts 
      median = (sort noCorruptsScore) !! (length noCorruptsScore `div` 2)
  print $ sum val
  print $ median

simpSwap :: Char -> Char
simpSwap x
  | x == '('  = ')'
  | x == '{'  = '}'
  | x == '<'  = '>'
  | x == '['  = ']'
  | otherwise = '!'

simpVal :: Char -> Int
simpVal x
  | x == ')'  = 3
  | x == '}'  = 1197
  | x == '>'  = 25137
  | x == ']'  = 57
  | otherwise = 0

simpVal2 :: Char -> Int
simpVal2 x
  | x == '('  = 1
  | x == '['  = 2
  | x == '{'  = 3
  | x == '<'  = 4
  | otherwise = 0  

parseCorrupt :: String -> String
parseCorrupt [] = []
parseCorrupt (x:xs)
  | isPrefixOf ([x] ++ [simpSwap x]) (x:xs) = parseCorrupt (drop 2 (x:xs))
  | otherwise                               = x : parseCorrupt xs

loopParse :: String -> String
loopParse x
  | x == parseCorrupt x = x
  | otherwise           = loopParse (parseCorrupt x)

earliestCorrupt :: String -> Char
earliestCorrupt corrupt = corrupt !! (minimum $ concatMap (`elemIndices` corrupt) ")}>]")