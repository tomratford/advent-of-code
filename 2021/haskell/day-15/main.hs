--2nd implementation of Djikstra's algorithm.

import Debug.Trace (trace)

import Data.List (delete)
import Data.Char (digitToInt)
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int,Int)
type Weight = Int
type Node = M.Map Coord Int --Weight == Distance from (0,0) to the Coord specified
type Unvisited = S.Set Coord
type Cost = M.Map Coord Int --The only thing this is used if for `!`

main = do
  putStrLn "File:"
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile
      crudeDigits = map (map digitToInt) fileLines
      --Setup for part 1
      unvisitedList = [ (x,y) | y <- [0..(length crudeDigits-1)],
                         x <- [0..(length (head crudeDigits)-1)]
                          ]
      unvisited = S.fromList unvisitedList
      nodes = M.fromList $ zip unvisitedList (0 : repeat (maxBound::Int))
      costMap = M.fromList $ zip unvisitedList (concat crudeDigits)
      endPoint = (length (head crudeDigits)-1, length crudeDigits-1)
      solMap = djikstra endPoint costMap unvisited nodes (0,0) --Part 1 sol
      sol = M.elemAt ((length (head crudeDigits) * length crudeDigits)-1) solMap
      --Pt2
      extendDigits = map extendRight (extendDown crudeDigits)
      xUnvisitedList = [ (x,y) | y <- [0..(length extendDigits-1)],
                            x <- [0..(length (head extendDigits)-1)]
                          ]
      xUnvisited = S.fromList xUnvisitedList
      xNodes = M.fromList $ zip xUnvisitedList (0 : repeat (maxBound::Int))
      xCostMap = M.fromList $ zip xUnvisitedList (concat extendDigits)
      xEndPoint = (length (head extendDigits)-1, length extendDigits-1)
      xSolMap = djikstra xEndPoint xCostMap xUnvisited xNodes (0,0)
      xSol = M.elemAt ((length (head extendDigits) * length extendDigits)-1) xSolMap
  print $ xSol

getMinFromMap :: Ord a1 => M.Map a2 a1 -> [a2]
getMinFromMap m = go [] Nothing (M.toList m)
  where
    go ks _        []           = ks
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v > u     = go ks     (Just u) rest
        | v < u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

djikstra :: Coord -> Cost -> Unvisited -> Node -> Coord -> Node
djikstra endPoint _ unvisited sol _
  | null unvisited = sol
  | endPoint `notElem` unvisited = sol
djikstra e cost unvisited sol coord = djikstra e cost newUnvisited newSol newCoord
  where newUnvisited = coord `S.delete` unvisited
        newSol = M.unionWith (\x y -> min (x + sol ! coord) y) (getNeighbours coord) sol
        getNeighbours :: Coord -> Cost
        getNeighbours (x,y) = cost `M.restrictKeys` adjCoords
          where adjCoords = S.fromList [(x+i,y+j)| i <- [1,0,-1], j <- [1,0,-1], i^2 /= j^2]
        newCoord = head $ getMinFromMap $ newSol `M.restrictKeys` newUnvisited

--Pt2 specific
extendRight :: [Int] -> [Int]
extendRight digits = concat [digits, addOne, addTwo, addThree, addFour]
  where addX n x = if (x+n) > 9 then x+n-9 else x+n
        addOne = map (addX 1) digits 
        addTwo = map (addX 2) digits
        addThree = map (addX 3) digits
        addFour = map (addX 4) digits

extendDown :: [[Int]] -> [[Int]]
extendDown digits = digits ++ addOne ++ addTwo ++ addThree ++ addFour
  where addX n x = if (x+n) > 9 then x+n-9 else x+n
        addOne = map (map (addX 1)) digits
        addTwo = map (map (addX 2)) digits
        addThree = map (map (addX 3)) digits
        addFour = map (map (addX 4)) digits