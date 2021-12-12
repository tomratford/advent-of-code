import Debug.Trace (trace)

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

simSchoolDay :: [LanternFish] -> Integer -> [LanternFish]
simSchoolDay x 0 = x
simSchoolDay x n = simSchoolDay newX (n-1)
  where newX = concatMap simDay x

--Pt2 Approach
--Operate on each fish only once. I want to get an output map that I then do the same too
simTime :: Integer -> Integer -> Integer
simTime daysLeft daysToFish
  | daysLeft - daysToFish <= 0 = 1
  | otherwise                  = simTime (daysLeft - daysToFish) 7 + simTime (daysLeft - daysToFish) 9

--simTime 18 3 = simTime 15 6                                          + simTime 15 8
--             = simTime 9 6               + simTime 9 8               + simTime 7 6               + simTime 7 8
--             = simTime 3 6 + simTime 3 8 + simTime 1 6 + simTime 1 8 + simTime 1 6 + simTime 1 8 + 1
--             = 7

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
      days = read daysInput :: Integer
      --Uncomment for pt2
      --fish = map OldFish nums
      --test = simSchoolDay fish days
  --print (length test)
  --pt2
  let test2 = map (simTime days) nums
  print (sum test2)