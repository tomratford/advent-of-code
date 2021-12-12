import Data.ByteString (elemIndices)
type Days = Integer
data LanternFish = NewFish Days | OldFish Days deriving (Show)

--Pt1 Approach
simDay :: LanternFish -> [LanternFish]
simDay (NewFish n)
  | n == 0    = [OldFish 6, NewFish 8]
  | otherwise = [NewFish (n-1)]
simDay (OldFish n)
  | n == 0    = [OldFish 6, NewFish 8]
  | otherwise = [OldFish (n-1)]

simSchoolDay :: [LanternFish] -> Int -> [LanternFish]
simSchoolDay x 0 = x
simSchoolDay x n = simSchoolDay newX (n-1)
  where newX = concatMap simDay x

--Pt2 Approach
simOneDay :: (Integral a) => [a] -> [a]
simOneDay [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,a+h,i,a]
simOneDay _                   = []

--Abridged words function to split at commas
commas   :: String -> [String]
commas s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : commas s''
                            where (w, s'') = break (==',') s'

main = do
  putStrLn "File:"
  userInput <- getLine
  putStrLn "Num days:"
  daysInput <- getLine
  ls <- readFile userInput
  
  let nums = map read $ commas ls :: [Integer]
      days = read daysInput :: Int
  --    --pt1
  --    fish = map OldFish nums
  --    test = simSchoolDay fish days
  --print (length test)
  --pt2
  let freq = map (\x -> length $ filter (==x) nums) [0,1,2,3,4,5,6,7,8]
      pt2  = iterate simOneDay freq !! days
  print (sum pt2)