import Data.List ((\\), intersect, union, sort, group)

type Coord = (Int,Int)
data Vector = ZeroVector | Vector Coord Coord deriving (Show)

seq' :: Int -> Int -> [Int]
seq' x y
  | x > y    = [x,x-1..y]
  | otherwise = [x..y]

zip' :: [a] -> [b] -> [(a,b)]
zip' (a:as) (b:bs)
  | length as < length bs = (a,b) : zip' (as ++ [a]) bs
  | length as > length bs = (a,b) : zip' as (bs ++ [b])
  | otherwise             = zip (a:as) (b:bs)

getPoints :: Vector -> [Coord]
getPoints ZeroVector   = []
getPoints (Vector x y) = zip' (seq' (fst x) (fst y)) (seq' (snd x) (snd y))

----Unused
--diffPoints :: Vector -> Vector -> [Coord]
--diffPoints ZeroVector _              = []
--diffPoints _          ZeroVector     = []
--diffPoints v1         v2             = intersect (getPoints v1) (getPoints v2)

--allPoints :: [Vector] -> [Coord]
--allPoints = concatMap getPoints

isStraight :: Vector -> Bool
isStraight ZeroVector = False
isStraight (Vector x y) = (fst x == fst y) || (snd x == snd y)

main = do
  userInput <- getLine
  ls <- readFile userInput
  strs <- return (lines ls)
  let ls = map (words . (\\ "->")) strs
      strCoords = map (map (\ x -> "(" ++ x ++ ")")) ls
      coords = map (map read) strCoords :: [[Coord]]
      vectors = map (\ x -> Vector (head x) (head $ tail x)) coords
      --Pt 1
      straightVectors = filter isStraight vectors
      allStraight = concatMap getPoints straightVectors
      uniqueStraightCnt = map (\xs@(x:_) -> (x, length xs)) . group . sort $ allStraight
      gt2StraightCnts = filter (\x -> snd x >= 2) uniqueStraightCnt
      --Pt 2
      all = concatMap getPoints vectors
      uniqueCnt = map (\xs@(x:_) -> (x, length xs)) . group . sort $ all
      gt2Cnts = filter (\ x -> snd x >= 2) uniqueCnt
  print (length gt2StraightCnts)
  print (length gt2Cnts)