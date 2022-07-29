import Data.List (nub, isPrefixOf, (\\), elemIndices)
import Data.Map ((!))
import qualified Data.Map as M

--Type decs
type Coord = (Int,Int)
type Matrix = (Int,Int,Int,Int) --We're in a 2d plane so no need for generalisation
type Instruction = (Char, Int)

main = do
  putStrLn "File:"
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile
      splitPoint = head $ elemIndices "" fileLines
      coords = map parseCoords (take splitPoint fileLines)
      instructions = map parseInstructions (drop (splitPoint+1) fileLines)
      pt1 = doInstructions [head instructions] coords
      x = minXY 'x' instructions
      y = minXY 'y' instructions
      pt2 = doInstructions instructions coords
  print $ length pt1
  print $ pt2
  putStrLn $ disp x y pt2

--Matrix * Coord mult.
mMult :: Matrix -> Coord -> Coord
mMult (a,b,c,d) (x,y) = (a*x + b*y, c*x + d*y)

xReflectMatrix :: Matrix
xReflectMatrix = (-1,0,
                   0,1)

yReflectMatrix :: Matrix
yReflectMatrix = (1,0,
                  0,-1)

--Coord addition
tAdd :: Coord -> Coord -> Coord
tAdd (x,y) (a,b) = (x+a,y+b)

--Reflect in x/y
yReflect :: Int -> Coord -> Coord
yReflect y coord
  | y < snd coord = mMult yReflectMatrix (coord `tAdd` (0,-y)) `tAdd` (0,y)
  | otherwise     = coord

xReflect :: Int -> Coord -> Coord
xReflect x coord
  | x < fst coord = mMult xReflectMatrix (coord `tAdd` (-x,0)) `tAdd` (x,0)
  | otherwise     = coord

--Parse line.
parseCoords :: String -> Coord
parseCoords x = read ("("++x++")") :: Coord

parseInstructions :: String -> Instruction
parseInstructions x = ((head . fst) parsed, read (snd parsed) :: Int)
    where parsed = (splitAt 2 . last . words) x

--Min instruction to work out dots.
minXY :: Char -> [Instruction] -> Int
minXY c instructions = minimum $ map snd $ filter (\x -> fst x==c) instructions

--Loop over instructions
doInstructions :: [Instruction] -> [Coord] -> [Coord]
doInstructions [] c = c
doInstructions (i:is) c
  | fst i == 'x' = doInstructions is (nub $ map (xReflect (snd i)) c)
  | otherwise    = doInstructions is (nub $ map (yReflect (snd i)) c)

--Pretty functions
--Attempt 1:
changeDot :: [String] -> Coord -> [String]
changeDot str (x,y) = reComb
  where ySplit = splitAt y str
        prevStrs = fst ySplit
        replaceStr = (head . snd) ySplit
        lastStrs = (tail . snd) ySplit
        xSplit = splitAt x replaceStr
        replaceChars = fst xSplit ++ "#" ++ (init . snd) xSplit
        reComb = prevStrs ++ [replaceChars] ++ lastStrs

printPretty :: Int -> Int -> [Coord] -> IO ()
printPretty x y coords = do
  let dots = replicate y (replicate x '.')
      pretty = foldl changeDot dots coords
  putStr $ unlines pretty

--Attempt 2:
recurUpdate :: M.Map Coord Char -> [Coord] -> M.Map Coord Char
recurUpdate map []        = map
recurUpdate map (c:oords) = recurUpdate (M.adjust f c map) oords
  where f x = '#'

recurSplit :: Int -> String -> [String]
recurSplit _ []  = []
recurSplit x str = fst split : recurSplit x (snd split)
  where split = splitAt x str


pretty :: Int -> Int -> [Coord] -> IO ()
pretty x y coords = do
  let allCoords = [(i,j) | j <- [0..x-1], i <- [0..y-1]]
      map = M.fromList $ zip allCoords (repeat '.')
      updated = recurUpdate map coords
      longString = fmap snd (M.toAscList updated)
      splitString = recurSplit x longString
  putStrLn $ unlines splitString

--Attempt 3:
disp :: Int -> Int -> [Coord] -> String
disp x y coords = unlines [ [if (x,y) `elem` coords then '#' else '.' | x <- [0..x-1]] | y <-[0..y-1]]