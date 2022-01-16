import Data.List ( group, sort, elemIndices, transpose, intersectBy )

--Pt1
groupList :: [[a]] -> [[a]]
groupList x
  | any null x = []
groupList x = map head x : groupList (map tail x)

getBinaryGamma :: [[Int]] -> [Int]
getBinaryGamma = foldr (\ x -> (++) (elemIndices (maximum x) x)) []

getBinaryEpsilon :: [[Int]] -> [Int]
getBinaryEpsilon = foldr (\ x -> (++) (elemIndices (minimum x) x)) []

binToDec :: [Int] -> Int
binToDec b = sum $ map (2^) $ elemIndices 1 $ reverse b

--Pt2
groupSortCount :: [String] -> [[Int]]
groupSortCount x = map ((map length . group) . sort) $ transpose x

getOxygenGenerator :: Int -> [String] -> [String]
getOxygenGenerator _ [x] = [x]
getOxygenGenerator n [x,y]
  | (x !! n) == '1' = [x]
  | (y !! n) == '1' = [y]
getOxygenGenerator n x = getOxygenGenerator (n+1) leftovers
  where intersectVal = map show $ getBinaryGamma $ groupSortCount x :: [String]
        leftovers    = intersectBy (\x y -> x !! n == head y) x [intersectVal !! n]

getC02Scrubber :: Int -> [String] -> [String]
getC02Scrubber _ [x] = [x]
getC02Scrubber n x = getC02Scrubber (n+1) leftovers
  where intersectVal = map show $ getBinaryEpsilon $ groupSortCount x :: [String]
        leftovers    = intersectBy (\x y -> x !! n == head y) x [intersectVal !! n]
        
binToDecStr :: String -> Int
binToDecStr b = sum $ map (2^) $ elemIndices '1' $ reverse b

main = do
  ls <- readFile "input.txt"
  strs <- return (lines ls)
  let grouped = groupList strs
      sorted = map sort grouped
      counts = map (map length . group) sorted
      gammaBin = getBinaryGamma counts
      epsilonBin = getBinaryEpsilon counts
      pt1 = binToDec gammaBin * binToDec epsilonBin
  print pt1
  print "---"
  let grpSrtCnt = groupSortCount strs
      headN = show $ head $ getBinaryGamma grpSrtCnt :: String
      valstokeep = elemIndices (head headN) (head grouped)
      intersectTest = intersectBy (\x y -> head x==head y) strs [headN]
      intersectVal = map show $ getBinaryGamma $ groupSortCount strs :: [String]
      leftovers    = intersectBy (\x y -> x !! 0 == head y) strs [intersectVal !! 0]
      oxygenBin = getOxygenGenerator 0 strs
      c02Bin = getC02Scrubber 0 strs
      pt2 = binToDecStr (head oxygenBin) * binToDecStr (head c02Bin)
  print pt2
