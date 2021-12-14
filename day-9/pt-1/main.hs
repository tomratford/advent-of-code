import Data.List (transpose)
import Data.Char (digitToInt)
import Debug.Trace (trace)

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

--Pt1
type Point = (Int,Int)

--getPoint takes verDigs
getPoint :: [[Int]] -> Point -> Int
getPoint verDigs (x,y) = verDigs !! x !! y

getNeighbours :: [[Int]] -> Point -> [Point]
getNeighbours vals (x,y) = [(x+i,y+j) | i <- [1,0,-1],
                                        j <- [1,0,-1],
                                        i /= j,
                                        i+j /= 0,
                                        x+i >= 0 && x+i <= horLen,
                                        y+j >= 0 && y+j <= verLen]
  where horLen = length vals -1
        verLen = length (head vals) -1

compPoints :: [[Int]] -> [Point] -> Int
compPoints vers [] = 0
compPoints vers (p:ps)
  | all ((>gP p) . gP) (getNeighbours vers p) = 1+ gP p + compPoints vers ps
  | otherwise = compPoints vers ps
    where gP = getPoint vers

main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  let ls = lines rawFile
      longInts = map (\x -> read x :: Integer) ls
      horDigs = map (map digitToInt) ls
      verDigs = transpose horDigs
      --pt1
      listPoints = [(x,y) | x <- [0..length (head horDigs)-1], y <- [0..length (head verDigs)-1]]
      sum = compPoints verDigs listPoints
  print $ sum