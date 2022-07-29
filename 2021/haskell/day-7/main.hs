--Abridged words function to split at commas
commas   :: String -> [String]
commas s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : commas s''
                            where (w, s'') = break (==',') s'

main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  let myInput = map read $ commas rawFile :: [Int]
      --Pt1
      valsToTest = [1..(maximum myInput)]
      getSumPt1 = map (sum . (\ y -> [abs . (`subtract` y) $ x | x <- myInput])) valsToTest
      --Pt2
      fuelCost = scanl (+) 0 valsToTest
      --getSumPt2 = map (\ y -> map (\ x -> fuelCost !! abs (x - y - 1)) myInput) valsToTest
      getSumPt2 = map (sum. (\ y -> map (\ x -> fuelCost !! abs (x - y)) myInput)) valsToTest
  print (minimum getSumPt1)
  print (minimum getSumPt2)