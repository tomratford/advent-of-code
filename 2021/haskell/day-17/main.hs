import Data.List (nub, (\\))

main = do
  putStr "File: "
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile
      fileWords = drop 2 $ words rawFile
      parsed = map parseBounds fileWords
      xT = head parsed
      yT = last parsed
      xRange = [1..snd xT]
  print $ pt1 yT
  print $ length $ getVel xT yT xRange [(fst yT)..(abs $ fst yT)]

parseNumsWithDots :: String -> String
parseNumsWithDots (x:y:[]) = [x,y]
parseNumsWithDots (x:y:xs)
  | ".." == [x,y] = ',':xs
  | otherwise     = x : parseNumsWithDots (y:xs)

parseBounds :: String -> (Int,Int)
parseBounds str = read parse :: (Int,Int)
  where dropBounds = drop 2 (str \\ ",")
        parse = "(" ++ parseNumsWithDots dropBounds ++ ")"

quickAdd :: Integral a => a -> a
quickAdd x
  | even x    = (x + 1) * half
  | otherwise = (x + 2) * half + 1
    where half = x `div` 2

pt1 :: (Int,Int) -> Int
pt1 (min,max) = quickAdd (abs min-1) --I can't believe this actually worked. 

--Pt2
atStep :: Integral a => a -> a -> a
atStep y step = quickAdd y - quickAdd (step - y - 1)

horStep :: Integral a => a -> a -> a
horStep x step = if step > x then quickAdd x else x `atStep` step

posStep :: Integral b => (b, b) -> b -> (b, b)
posStep (x,y) step = (horStep x step, atStep y step)

inBound :: Ord a => a -> (a, a) -> Bool
inBound x (low,up) = low <= x && x <= up

--Brute force.

--Don't use
viableXRange :: (Int,Int) -> [Int]
viableXRange (lowX,highX) = [newLowX..highX]
  where newLowX = minimum $ filter ((>lowX) . quickAdd) [1..highX]

--Don't use
viableYRange :: [Int] -> (Int,Int) -> [Int]
viableYRange xRange (highY,lowY) = [highY..maximum xRange]

getVel :: (Int,Int) -> (Int,Int) -> [Int] -> [Int] -> [(Int,Int)]
getVel xTarget yTarget viableX viableY = out
  where steps = [1..maximum viableY*3] --Literally guessing the range
        out = nub $ [(x,y) | 
          x <- viableX,
          y <- viableY, 
          s <- steps, 
          x `horStep` s `inBound` xTarget, 
          y `atStep` s `inBound` yTarget]