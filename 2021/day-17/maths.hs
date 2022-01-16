import Data.List (nub, (\\))

main = do
  putStr "File: "
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile
      fileWords = drop 2 $ words rawFile
      parsed = map parseBounds fileWords
      checkWithBound = checkValidVert (head parsed) (last parsed)
      vertVals = concat [checkWithBound y | y <- [1..100]]
  print $ length vertVals

parseNumsWithDots :: String -> String
parseNumsWithDots (x:y:[]) = [x,y]
parseNumsWithDots (x:y:xs)
  | ".." == [x,y] = ',' : xs
  | otherwise     = x : parseNumsWithDots (y:xs)

-- parseBounds :: String -> (Float, Float)
parseBounds str = read parse :: (Int,Int)
  where dropBounds = drop 2 (str \\ ",")
        parse = "(" ++ parseNumsWithDots dropBounds ++ ")"

--Purely mathematical functions

--Solve 2nd power polynomial
-- polynomialSolver :: Float -> Float -> Float -> [a]
polynomialSolver a0 b0 c0 = [((-(2*b)) + sqrt(b^2 - 4*a*c)) `div` (2*a), ((-2*b) - sqrt(b^2 - 4*a*c)) `div` (2*a)]
  where a = fromIntegral a0
        b = fromIntegral b0
        c = fromIntegral c0

--Quickly calculate n+(n-1)+(n-2)+...+1
--O(1) and space(1)
quickAdd :: Integral a => a -> a
quickAdd x
  | even x    = (x + 1) * half
  | otherwise = (x + 2) * half + 1
    where half = x `div` 2

--Derivation funcs
atStep :: Integral a => a -> a -> a
atStep y step = quickAdd y - quickAdd (step - y - 1)

horStep :: Integral a => a -> a -> a
horStep x step = if step > x then quickAdd x else x `atStep` step

inBound :: Ord a => a -> (a, a) -> Bool
inBound x (low,up) = low <= x && x <= up

-- getVertTimeBound :: Float -> (Float, Float) -> (Float, Float)
getVertTimeBound u (highY,lowY) = (l,h)
  where h = (floor . last) $ polynomialSolver 1 (-(2*u)) highY
        l = (floor . last) $ polynomialSolver 1 (-(2*u)) lowY


-- checkValidVert :: (Float, Float) -> (Float, Float) -> Float -> Bool
checkValidVert (lowX, highX) (highY, lowY) u = getHorSpeed
  where pairToList (x,y) = [x,y]
        vTimeBound = nub $ pairToList $ getVertTimeBound u (highY, lowY)
        getHorSpeed = [x | t <- vTimeBound, x <- [1..highX], (x `horStep` t) `inBound` (lowX,highX)]
