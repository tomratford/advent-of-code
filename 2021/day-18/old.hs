import Debug.Trace (trace)

data Tree = Node Tree Tree | Leaf Int deriving (Eq) 

instance Show Tree where
  show (Leaf n) = show n
  show (Node t1 t2) = "[" ++ show t1 ++ "," ++ show t2 ++ "]"

main = do
  putStr "File: "
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  print "a"

--Fun tree function time
--isNode
isNode :: Tree -> Bool
isNode (Node _ _) = True
isNode x          = False

--Addition
treeAdd :: Tree -> Tree -> Tree
treeAdd t1 t2 = Node t1 t2 

--Get height (To test if we need to explode)
height :: Tree -> Int
height (Leaf _) = 0
height (Node t1 t2) = 1 + max (height t1) (height t2)

--Tree rotations
rotateLeft :: Tree -> Tree
rotateLeft (Node (Node (Leaf x1) (Leaf x2)) t2) = (Node (Node (Leaf x1) (Leaf x2)) t2)
rotateLeft (Node (Node (Leaf x1)       xt)  t2) = (Node xt (Node t2 (Leaf x1)))
rotateLeft (Node (Node xt        (Leaf x2)) t2) = (Node xt (Node (Leaf x2) t2))
rotateLeft (Node (Node xt1            xt2)  t2) = (Node xt1 (Node xt2 t2))

rotateRight :: Tree -> Tree
rotateRight (Node t1 (Node (Leaf x1) (Leaf x2))) = (Node t1 (Node (Leaf x1) (Leaf x2)))
rotateRight (Node t1 (Node (Leaf x1)       xt))  = (Node (Node t1 (Leaf x1)) xt)
rotateRight (Node t1 (Node xt        (Leaf x2))) = (Node (Node (Leaf x2) t1) xt)
rotateRight (Node t1 (Node xt1             xt2)) = (Node (Node t1 xt1) xt2)

--Part 1 problem funcs

--Explode
--TODO: this is the hard bit
--Plan: We find the directions needed to get to the lowest leftmost 4 depth.
--      We then rotate this way so that we have our explosion on the left/right and our tree on the right/left
--      We have a function that takes 2 trees (i.e our left and right node of above) and does the explosion and replaces the left with 0
--      We then invert the instructions and rotate accordingly
--Functions needed: get leftmost explosion instructions (i.e a list [] of strings)
--                  rotate tree from string
--                  explode
--                  invert rotation

--Recursively generate all possible Strings
simpComp :: Int -> [String]
simpComp 1 = ["L","R"]
simpComp height = [x ++ y | x <- simpComp (height-1), y <- simpComp 1]

--Get tree from String
navigateTree :: Tree -> String -> Tree
navigateTree tree [] = tree
navigateTree (Leaf a) (d:ir) = (Leaf (-1))
navigateTree (Node a b) (d:ir)
  | d == 'L' = navigateTree a ir
  | d == 'R' = navigateTree b ir
  | otherwise = (Leaf (-1))

getDir :: Tree -> [String]
getDir tree = dirs
  where h = height tree
        allroutes = simpComp (h-1)
        quickNav = navigateTree tree
        dirs = filter (isNode . quickNav) allroutes

rotateFromString :: Tree -> String -> Tree
rotateFromString tree [] = tree
rotateFromString tree (d:ir)
  | d == 'R' = rotateFromString (rotateRight tree) ir
  | d == 'L' = rotateFromString (rotateLeft tree) ir

rotateBack :: Tree -> String -> Tree
rotateBack tree [] = tree
rotateBack tree (d:ir)
  | d == 'R' = rotateFromString (rotateLeft tree) ir
  | d == 'L' = rotateFromString (rotateRight tree) ir

getLeaf :: Tree -> Int
getLeaf (Leaf x) = x
getLeaf x = trace ("ERROR: EXHAUSTIVE getLeaf") 0

replaceRightMost :: Tree -> Int -> Tree
replaceRightMost (Node a b) val = if isNode b then (Node a (replaceRightMost b val)) else (Node a (Leaf ((getLeaf b) + val)))
replaceRightMost x v = trace ("ERROR: EXHAUSTIVE replaceRightMost" ++ show x) x

replaceLeftMost :: Tree -> Int -> Tree
replaceLeftMost (Node a b) val = if isNode a then (Node (replaceLeftMost a val) b) else (Node (Leaf ((getLeaf a) + val)) b)
replaceLeftMost x v = trace ("ERROR: EXHAUSTIVE replaceLeftMost" ++  show x) x

--Fn for tree in form [x,y]/\[..] or vice verse
explodeLeft :: Tree -> Tree
explodeLeft (Node (Node (Leaf a) (Leaf b)) y) = replaceLeftMost y a
explodeLeft (Node x (Node (Leaf a) (Leaf b))) = replaceLeftMost x a
--Exchaustive check
explodeLeft x = trace ("ERROR: EXHAUSTIVE explodeTree" ++ show x) x

explodeRight :: Tree -> Tree
explodeRight (Node (Node (Leaf a) (Leaf b)) y) = replaceRightMost y b
explodeRight (Node x (Node (Leaf a) (Leaf b))) = replaceRightMost x b
--Exchaustive check
explodeRight x = trace ("ERROR: EXHAUSTIVE explodeTree" ++ show x) x

explode :: Tree -> Tree
explode tree
  | height tree <= 4 = tree
  | otherwise = exploded
    where rotateInstructions = getDir tree
          treeToExplode = rotateFromString tree (head rotateInstructions)
          exploded = treeToExplode

--Split >10
split :: Tree -> Tree
split (Leaf x)
  | x >= 10 = (Node (Leaf (x `quot` 2)) (Leaf (x `quot` 2 + (x `rem` 2))))
  | otherwise = (Leaf x)
splitPairs (Node t1 t2) = (Node (split t1) (split t2))  

--Magnitude
magnitude :: Tree -> Int
magnitude (Leaf x) = x
magnitude (Node t1 t2) = 3*magnitude t1 + 2*magnitude t2

--Example trees
--[[[[[9,8],1],2],3],4]
ex1 :: Tree
ex1 = Node
        (Node
          (Node
            (Node
              (Node
                (Leaf 9)
                (Leaf 8)
              )
              (Leaf 2)
            )
            (Leaf 1)
          )
          (Leaf 3)
        )
        (Leaf 4)

--[[6,[5,[4,[3,2]]]],1]
ex2 :: Tree
ex2 = Node
        (Node
          (Leaf 6)
          (Node
            (Leaf 5)
            (Node
              (Leaf 4)
              (Node
                (Leaf 3)
                (Leaf 2)
              )
            )
          )
        )
        (Leaf 1)

--[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]
ex3 :: Tree
ex3 = Node
        (Node
          (Leaf 3)
          (Node
            (Leaf 2)
            (Node 
              (Leaf 1)
              (Node
                (Leaf 7)
                (Leaf 3)
              )
            )
          )
        )
        (Node
          (Leaf 6)
          (Node
            (Leaf 5)
            (Node 
              (Leaf 4)
              (Node
                (Leaf 3)
                (Leaf 2)  
              )
            )
          )
        )