import Data.List (transpose, nub)

main = do
  putStr "File: "
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile
      parsed = map parse fileLines
      loop = iterate mainLoop parsed
  print $ firstDup 0 loop

data Cucumber = South | East | Empty deriving (Show, Eq)

parse :: String -> [Cucumber]
parse [] = []
parse (x:xs)
  | x == '.' = Empty : parse xs
  | x == '>' = East : parse xs
  | otherwise = South : parse xs

checkEast :: [Cucumber] -> [Cucumber]
checkEast [] = []
checkEast [c] = [c]
checkEast (c:u:mb)
  | c == East && u == Empty = u:c:checkEast mb
  | otherwise = c:checkEast (u:mb)

checkSouth :: [Cucumber] -> [Cucumber]
checkSouth [] = []
checkSouth [c] = [c]
checkSouth (c:u:mb)
  | c == South && u == Empty = u:c:checkEast mb
  | otherwise = c:checkEast (u:mb)

mainLoop :: [[Cucumber]] -> [[Cucumber]]
mainLoop cucumbers = new
  where east = map checkEast cucumbers
        trans = transpose east
        south = map checkSouth cucumbers
        new = transpose south

firstDup :: (Eq a) => Int -> [[a]]-> Int
firstDup i (x:y:xs) = if x==y then i else firstDup (i+1) (y:xs)