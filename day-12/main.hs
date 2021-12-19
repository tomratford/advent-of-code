import Data.Map ((!))
import qualified Data.Map as M --You can think of this as a module of associative array
import Data.List (nub, (\\), elemIndices, sort, group) --nub removes dups. (\\) removes values from a list
import Data.Char (isUpper)

--Type declarations. 
-- We can create our own data types. Such as one for each cave. Deriving allows us to use this in certain ways (such as printing (show) or comparisons (eq/ord))
data Cave = Start | End | Small String | Big String deriving (Show, Eq, Ord)
type Edge = (Cave, Cave) --This is a 'tuple' of 2 caves, tuples are both good and annoying. This is almost exclusively used to make parsing easier.
type Graph = M.Map Cave [Cave] --Graphs are arrays with keys of each cave element and values all possible locations they can go
type Path = M.Map Int Cave --A path is an arrays of their order (Int) and the cave visited

--Main loop
main = do
  putStrLn "File:"
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let fileLines = lines rawFile --Split string into lines
      edges = map parseEdges fileLines --Look below for parseEdges dec. This just makes our edges
      caves = getCaves edges --This looks over all our edges and gets our caves
      tupNodes = map (makeNode edges) caves --Look below for makeNode dec. This turns our edges and caves into (Cave, [Possible locations]) to be read as an aray
      graph = M.fromList tupNodes --Make 'array'
      pt1Loop = nextStep graph (M.fromList [(1,Start)]) --Our recursive loop for part 1
      pt2Loop = nextStep' graph (M.fromList [(1,Start)]) --Our recursive loop for part 2
  print $ length pt1Loop
  print $ length pt2Loop

--Supplementary tuple funcions to make reading edges easier

--Check if an element is in a tuple
tupElem :: Eq a => a -> (a,a) -> Bool 
--Eq a means that type 'a' has the ability to be equal to something. 
-- then we have a function taking two args. A singular element of type a and a tuple with both elements type a
tupElem e (x,y) = e == x || e == y -- Our actual function all.

--Converts tup to list
tupCat :: (a,a) -> [a] 
tupCat (x,y) = [x,y]

--This returns the other value in a tup.
getDest :: Eq a => a -> (a,a) -> a 
getDest a (x,y)
  | a == x    = y --These are called guards and can be used to match patterns in values
  | a == y    = x
  | otherwise = a

--Generic function to split a string
splitAt'     :: (Char -> Bool) -> String -> [String]
splitAt' f s =  case dropWhile f s of
                      "" -> []
                      s' -> w : splitAt' f s''
                            where (w, s'') = break f s'

--parses string to cave
parseCave :: String -> Cave
parseCave x
  | x == "start"  = Start
  | x == "end"    = End
  | all isUpper x = Big x 
  | otherwise     = Small x

--returns our cave type
caveType :: Cave -> String
caveType Start     = "Start"
caveType End       = "End"
caveType (Big x)   = "Big"
caveType (Small x) = "Small"

--This turns a line from the input into a edge (Tuple (Cave, Cave))
--Strings are just a list of chars. Lists are linked with head (first element) & tail (every other element)
-- dot notation works the same as in maths. (head . tail) == head(tail())
-- Map loops over every input in a list and applies a functon. 
parseEdges :: String -> Edge
parseEdges x = (head parsedList, (head . tail) parsedList) 
  where parsedList = (map parseCave . splitAt' (=='-')) x 

--concatMap maps over a list but then concatenates the output lists together
getCaves :: [Edge] -> [Cave]
getCaves = nub . concatMap tupCat

--This makes our edge into a node
makeNode :: [Edge] -> Cave -> (Cave,[Cave])
makeNode edges cave = (cave,linkedEdges)
  where linkedEdges = nub (map (getDest cave) edges) \\ [Start, cave] --We dont want a cave to include itself or the start as it can't go there
--getDest cave is the first example of 'currying'. getDest take 2 args (a -> (a,a) -> a) but in haskell you can curry a function.
--Above we only give the first input. And now we have a function of type ((a,a) -> a) becuase we have already provided the first input

--Pt1
--Checks if a small cave has been visited
isVisited :: Path -> Cave -> Bool
isVisited path cave = visitNum >= 2
  where pathVals = (map snd . M.toList) path --gets all vals in our path currently
        visitNum = length $ elemIndices cave pathVals 
        --elemIndices returns the positions a value (cave) returns in a list (pathVals).
        --Length then tells us how many elements are in this list. If the cave inputted appears twice we return true.

--Recursive loop
nextStep :: Graph -> Path -> [Path]
nextStep graph curPath
  | mostRecentType == "End" = [curPath] --If the most recent cave visited is end return our path because we've finished
  | mostRecentType == "Small" && isVisited curPath mostRecentCave = [] --If most recent cave is small & we've already visited it then 'delete' this list (return an empty [])
  | otherwise             = concatMap (\ x -> nextStep graph $ M.insert (length curPath + 1) x curPath) possEdges --M.insert key val map 
  --Our recursive loop. This uses lambda calculus (\ x -> ) which is basically a simplified function call. 
  --We add every possible edge to our list of paths. We remove them later if they are incorrect.
  where mostRecentCave = curPath ! length curPath --our most recent cave visited (last element of path)
        mostRecentType = caveType mostRecentCave --Its type
        possEdges      = graph ! mostRecentCave --and then the possible edges it can have

--Pt2
--Return true if we have already visited a small cave twice
smallCaveCount :: Path -> Bool
smallCaveCount path = length (filter (>1) counts) > 1 || maximum counts > 2
  where pathVals = (map snd . M.toList) path
        counts = (map length . group . sort) $ filter (\ x -> caveType x == "Small") pathVals

nextStep' :: Graph -> Path -> [Path] --Same as above just with a differnet functions
nextStep' graph curPath
  | mostRecentType == "End" = [curPath]
  | mostRecentType == "Small" && smallCaveCount curPath = []
  | otherwise             = concatMap (\ x -> nextStep' graph $ M.insert (length curPath + 1) x curPath) possEdges
  where mostRecentCave = curPath ! length curPath
        mostRecentType = caveType mostRecentCave
        possEdges      = graph ! mostRecentCave