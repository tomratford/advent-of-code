import Data.List (transpose, sort, nub)
import Data.Char (digitToInt)
import Debug.Trace (trace)

type Point = (Int,Int)

getVal :: [[Int]] -> Point -> Int
getVal digits (x,y) = digits !! x !! y

getNeighbours :: [[Int]] -> Point -> [Point]
getNeighbours vals (x,y) = [(x+i,y+j) | i <- [1,0,-1],
                                        j <- [1,0,-1],
                                        i /= j,
                                        i+j /= 0,
                                        x+i >= 0 && x+i <= horLen,
                                        y+j >= 0 && y+j <= verLen]
  where horLen = length vals -1
        verLen = length (head vals) -1

lowPoints :: [[Int]] -> [Point] -> [Point]
lowPoints vers [] = []
lowPoints vers (p:ps)
  | all ((>gP p) . gP) (getNeighbours vers p) = p : lowPoints vers ps
  | otherwise = lowPoints vers ps
    where gP = getVal vers

getBasin :: [[Int]] -> [Point] -> [[Point]]
getBasin digits [] = []
getBasin digits (p:ps) = [p : nub (concat $ concatMap (getBasin digits) [basinVals])] ++ getBasin digits ps
  where basinVals9 = filter ((>(getVal digits p)) . getVal digits) (getNeighbours digits p)
        basinVals = filter ((/=9) . getVal digits) basinVals9

main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  let ls = lines rawFile
      longInts = map (\x -> read x :: Integer) ls
      horDigs = map (map digitToInt) ls
      verDigs = transpose horDigs      
      listPoints = [(x,y) | x <- [0..length (head horDigs)-1], y <- [0..length horDigs-1]]
      lowPts = lowPoints verDigs listPoints
      basins = getBasin verDigs lowPts
      basLen = map length basins
  print $ product (take 3 . reverse $ sort basLen)