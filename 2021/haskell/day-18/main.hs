import Debug.Trace (trace)
import Text.Parsec ( char, digit, (<|>), parse )
import System.IO (isEOF)

--Main loop
main = do
  putStr "File: "
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let numbers = map parseLine (lines rawFile)
      part1 = foldl1 add2 numbers
  print part1
  print $ magnitude part1
  let part2 = getAllComb numbers
      part2Mag = map magnitude part2
  print $ maximum part2Mag
--Tree stuff
data Tree = Node Tree Tree | Leaf Int deriving (Eq, Ord)
instance Show Tree where
  show (Leaf n) = show n
  show (Node t1 t2) = "[" ++ show t1 ++ "," ++ show t2 ++ "]"

--'Context' see README
data Cxt = Top | L Cxt Tree | R Tree Cxt deriving (Show)

type Loc = (Tree, Cxt)

--Movement functions
left :: Loc -> Loc
left (Leaf x, c) = (Leaf x, c)
left (Node l r, c) = (l, L c r)

right :: Loc -> Loc
right (Leaf x, c) = (Leaf x, c)
right (Node l r, c) = (r, R l c)

top :: Tree -> Loc
top t = (t, Top)

up :: Loc -> Loc
up (t, L c r) = (Node t r, c)
up (t, R l c) = (Node l t, c)
up (t, Top)   = (t, Top)

upmost :: Loc -> Loc
upmost l@(t, Top) = l
upmost l = upmost (up l)

modify :: Loc -> (Tree -> Tree) -> Loc
modify (t, c) f = (f t, c)

findRightNum :: Loc -> Loc
findRightNum (t, Top)     = (t, Top)
findRightNum t@(_, L c r) = goLeft $ (right . up) t
findRightNum t@(_, R l c) = findRightNum $ up t

goLeft :: Loc -> Loc
goLeft t@(Leaf _, _)   = t
goLeft t@(Node _ _, _) = goLeft $ left t

findLeftNum :: Loc -> Loc
findLeftNum (t, Top)     = (t, Top)
findLeftNum t@(_, R l c) = goRight $ (left . up) t
findLeftNum t@(_, L c r) = findLeftNum $ up t

goRight :: Loc -> Loc
goRight t@(Leaf _, _) = t
goRight t@(Node _ _, _) = goRight $ right t

--Tree funcs
--isNode
isNode :: Tree -> Bool
isNode (Node _ _) = True
isNode x          = False

--Addition
treeAdd :: Tree -> Tree -> Tree
treeAdd t1 t2 = Node t1 t2 

--Tree height
height :: Tree -> Int
height (Leaf _) = 0
height (Node t1 t2) = 1 + max (height t1) (height t2)

maxVal :: Tree -> Int
maxVal (Leaf x)     = x
maxVal (Node t1 t2) = max (maxVal t1) (maxVal t2) 

--Part 1 funcs
simpComp :: Int -> [Loc -> Loc]
simpComp 1 = [left,right]
simpComp height = [y . x | x <- simpComp (height-1), y <- simpComp 1]

getDir :: Tree -> [Loc]
getDir tree = dirs
  where loc = top tree
        h = height tree
        allroutes = simpComp (h-1)
        dirs = filter (isNode . fst) $ map (\x -> x $ loc) allroutes

getDirRoute :: Tree -> (Loc -> Loc)
getDirRoute tree = dirRoute
  where loc = top tree
        h = height tree
        allroutes = simpComp (h-1)
        dirRoute= head $ filter (\x -> (isNode . fst) $ x loc) allroutes

nodeToZero :: Tree -> Tree
nodeToZero (Leaf x) = Leaf 0
nodeToZero (Node _ _) = Leaf 0

getVal :: Tree -> [Int]
getVal (Leaf x)   = [x]
getVal (Node x y) = getVal x ++ getVal y

addVal :: Tree -> Int -> Tree
addVal (Node x y) _ = (Node x y)
addVal (Leaf x)   n = (Leaf (x+n))

explode :: Tree -> Tree
explode tree = newTree
        --Get explode vals
  where loc = head $ getDir tree
        explodeVals = getVal (fst loc)
        leftVal = head explodeVals
        rightVal = last explodeVals
        --Start loc & get route from top
        --Go left
        goToLeftMost = findLeftNum loc
        addToLeft = modify goToLeftMost (\t -> t `addVal` leftVal)
        --Back up
        backToTop = upmost addToLeft
        backDown = head $ getDir (fst backToTop)
        --Go right
        goToRightMost = findRightNum backDown
        addToRight = modify goToRightMost (\t -> t `addVal` rightVal)
        --Back up
        backToTop2 = upmost addToRight
        backDown2 = head $ getDir (fst backToTop2)
        --Set as 0
        setAs0 = modify backDown2 (\t -> Leaf 0)
        newTree = fst (upmost setAs0) -- fin.

--Split >10
allComp :: Int -> [Loc -> Loc]
allComp 1 = simpComp 1
allComp height = simpComp height ++ allComp (height-1)

getSplit :: Tree -> [Loc]
getSplit tree = dirs
  where loc = top tree
        h = height tree
        allroutes = allComp h
        dirs = filter (\x -> (not . isNode . fst) x && (head (getVal $ fst x) > 9)) $ map (\x -> x $ loc) allroutes

split :: Tree -> Tree
split t = fst upTree
  where splTree = getSplit t
        tree2 = if null splTree then (Leaf 1,Top) else head splTree
        aux l@(Leaf v)
          | v >= 10 = (Node (Leaf (v `quot` 2)) (Leaf (v `quot` 2 + (v `rem` 2))))
          | otherwise = l
        aux t = t
        splitNode = modify tree2 aux
        upTree = upmost splitNode

--Magnitude
magnitude :: Tree -> Int
magnitude (Leaf x) = x
magnitude (Node t1 t2) = 3*magnitude t1 + 2*magnitude t2

explodeLoop :: Tree -> Tree
explodeLoop tree
  | height explodeTree > 4 = explodeLoop explodeTree
  | otherwise              = explodeTree
    where explodeTree = explode tree

--Reduce loop
reduce :: Tree -> Tree
reduce tree = if ((height splT > 4) || (split splT /= (Leaf 1))) then reduce splT else splT
  where expT = if height tree > 4 then explodeLoop tree else tree
        splT = if maxVal expT >= 10 then split expT else expT

add2 :: Tree -> Tree -> Tree
add2 tree1 tree2 = reduce (tree1 `treeAdd` tree2)

--Parser: Taken from (see README)
pSnailfish = pLeaf <|> pBranch

pLeaf = do
    n <- digit
    return (Leaf (read [n]))

pBranch = do
    char '['
    left <- pSnailfish
    char ','
    right <- pSnailfish
    char ']'
    return (Node left right)

parseLine :: String -> Tree
parseLine s = case parse pSnailfish "(input)" s of
    Left e -> error ("Parse error: " ++ show e)
    Right snailfish -> snailfish

--Part 2
getAllComb :: [Tree] -> [Tree]
getAllComb trees = leftRight ++ rightLeft
  where leftRight = [add2 x y | x <- trees, y <- trees, x/=y]
        rightLeft = [add2 y x | x <- trees, y <- trees, x/=y]