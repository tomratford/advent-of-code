--Dumb cheesy method which didnt work

import Data.List (nub, (\\))

main = do
  putStr "File: "
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile
      fileWords = drop 2 $ words rawFile
      parsed = map parseBounds fileWords
  print $ part1 (head parsed) (last parsed)

parseNumsWithDots :: String -> String
parseNumsWithDots (x:y:[]) = [x,y]
parseNumsWithDots (x:y:xs) 
  | ".." == [x,y] = ',':xs
  | otherwise     = x : parseNumsWithDots (y:xs)

parseBounds :: String -> (Int,Int)
parseBounds str = read parse :: (Int,Int)
  where dropBounds = drop 2 (str \\ ",")
        parse = "(" ++ parseNumsWithDots dropBounds ++ ")"

--Pt1 thought process
--If x+(x-1)+(x-2)+...+1 is greater than the minimum of the x bounds. Then at

--Quickly calculate n+(n-1)+(n-2)+...+1
--O(1) and space(1)
quickAdd :: Int -> Int
quickAdd x
  | even x    = (x + 1) * half
  | otherwise = (x + 2) * half + 1
    where half = x `div` 2
--Crude slow exact recursive call for checking
checkSum :: Int -> Int
checkSum 0 = 0
checkSum x = x + checkSum (x-1)

atStep :: Int -> Int -> Int
atStep y step = quickAdd y - quickAdd (step - y - 1)

inBound :: Int -> (Int,Int) -> Bool
inBound x (low,up) = low <= x && x <= up

--Assume we dont want negative x/negative y
crudeComprehension :: [(Int,Int)]
crudeComprehension = nub [(x, y) |
   x <- [0 .. 65],
   y <- [0 .. 300]]

checkSteps :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int) -> [Int]
checkSteps steps (lowX, highX) (highY, lowY) (x,y) = [ y | 
  s <- [0..steps],
  x `atStep` s `inBound` (lowX, highX),
  y `atStep` s `inBound` (highY, lowY) ]


part1 :: (Int,Int) -> (Int,Int) -> Int
part1 x y = maximum $ map quickAdd $ concatMap (checkSteps 20 x y) crudeComprehension