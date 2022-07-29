import Data.List (groupBy, transpose, (\\))
--Notes on solving:
-- checkWin -> Returns if a board wins. We can use this in a function guard
-- we can now check if an individual board wins. So we map this every time with a progressively bigger list every time. Using take?

--Abridged words function to split at commas
commas   :: String -> [String]
commas s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : commas s''
                            where (w, s'') = break (==',') s'

--Checks a win for an individual board
checkWin :: [[Int]] -> [Int] -> Bool
checkWin x y = any (all (`elem` y)) x || any (all (`elem` y)) (transpose x)

--Get the winning board, else []
getWinningBoard :: [[Int]] -> [Int] -> [Int]
getWinningBoard x y
  | checkWin x y = concat x \\ y
  | otherwise    = []

getLosingBoard :: [[Int]] -> [Int] -> [Int]
getLosingBoard x y
  | not $ checkWin x (tail $ reverse y) = getWinningBoard x y
  | otherwise          = []  

getWinner :: [[[Int]]] -> [Int] -> Int -> Int
getWinner [] _ _ = 0
getWinner x nums n
  | any (`checkWin` tempNums) x = (nums !! (n-1)) * sum (concat (map (`getWinningBoard` tempNums) x))
  | otherwise                   = getWinner x nums (n+1)
  where tempNums = take n nums

getLoser :: [[[Int]]] -> [Int] -> Int -> Int
getLoser [] _ _ = 0
getLoser x nums n
  | any (\x -> not $ x `checkWin` tempNums) x = (rNums !! (n-1)) * sum (concat (map (`getLosingBoard` (take (len-n+1) nums)) x))
  | otherwise                   = getLoser x nums (n+1) 
  where len = length nums -- I could not get it to work with tempNums so I used len-n+1 to get the nums
        rNums = reverse nums
        tempNums = drop n rNums

main = do
  ls <- readFile "input.txt"
  strs <- return (lines ls)
  let nums = map read $ commas (head strs) :: [Int]
      badBoard = groupBy (\x y -> not (null x || null y)) $ map words (tail strs)
      board = [ map (map read) a :: [[Int]] | a <- badBoard, a /= [[]]]
      winner = getWinner board nums 0 
  print winner
  let loser = getLoser board nums 0
  print loser