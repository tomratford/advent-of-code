import qualified Data.Set as Set
import Data.Map ((!))
import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.List (elemIndex, sortBy, minimumBy, (\\))
import Debug.Trace (trace)

--Type decs
type Coord = (Int,Int)
type Djiksta = M.Map Coord Int
type Cost = Djiksta
type Sol = Djiksta
type Unvisited = [Coord]

main = do
  putStrLn "File:"
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
      --Init read & parse of data
  let fileLines = lines rawFile
      crudeDigits = map (map digitToInt) fileLines
      --Setup constants
      allCoords = [(x,y) | y <- [0..(length crudeDigits-1)], x <- [0..(length (head crudeDigits)-1)]]
      crudeInitSol = 0 : repeat inf
      searchOrder = sortBy (\a@(x1,y1) b@(x2,y2) -> (x1 + y1) `compare` (x2 + y2)) allCoords
      --Setup boards
      costBoard = M.fromList $ zip allCoords (concat crudeDigits)
      initSol = M.fromList $ zip allCoords crudeInitSol
      unvisitedSet = Set.fromAscList (tail allCoords)
      --Run the loop
      --sols = foldl (djikstraStep costBoard) (tail allCoords, initSol) (tail searchOrder)
      sols2 = djikstraRec (length (head crudeDigits)-1, length crudeDigits-1) costBoard allCoords initSol (0,0)
  print $ sols2

inf :: Int
inf = maxBound::Int

--I think this is Djikstra's? So expect this to be a poor implementation of that.

getNeighbours :: Coord -> [Coord]
getNeighbours (x,y) = [(x+i,y+j)| i <- [1,0,-1], j <- [1,0,-1],
                                  i^2 /= j^2]

--Djikstra foldl implementation TODO: DOESNT WORK
djikstraStep :: Cost -> (Unvisited,Sol) -> Coord -> (Unvisited,Sol)
djikstraStep cost (unvisited,sol) coord = (newUnvisited,newSol)
  where coordNeighbours = M.filterWithKey (\k _ -> k `elem` getNeighbours coord) sol
        minNeighbour = M.findMin coordNeighbours
        newSol = M.adjustWithKey (\ k v -> let newval = snd minNeighbour + cost ! k in if newval < v then newval else v) coord sol
        newUnvisited =unvisited \\ [coord]

--Djikstra pure recursive implementation. Init with (0,0) as the last coord
djikstraRec :: Coord -> Cost -> Unvisited -> Sol -> Coord -> Sol
djikstraRec endPoint _ unvisited sol _
  | null unvisited = sol
  | endPoint `notElem` unvisited = sol
djikstraRec endPoint cost unvisited sol coord = djikstraRec endPoint cost newUnvisited newSol newCoord
  where coordNeighbours = M.filterWithKey (\k _ -> k `elem` getNeighbours coord && k `elem` unvisited) cost --Only get neighbours in coord and the unvisited set so we dont loop back
        newCoordVal k v = let newVal = (sol ! coord) + (cost ! k) in if k `M.member` coordNeighbours && newVal < v then newVal else v
        newSol = M.foldlWithKey (\ r k v -> M.adjustWithKey newCoordVal k r) sol coordNeighbours --Update sol with cost + current coord 
        newUnvisited = unvisited \\ [coord]
        newCoord = fst $ minimumBy (\x y -> snd x `compare` snd y) $ M.toList $ M.filterWithKey (\k _ -> k `elem` newUnvisited) newSol
