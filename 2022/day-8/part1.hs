module Main where

import Data.List (nub, transpose)
import System.Environment (getArgs)

-- Parse file initially
main = do
  input <- readFile . head =<< getArgs
  let parsed = (fmap parse . lines) input
      ext = exteriorTrees parsed
      trees = length $ part1 parsed
  putStr $ show $ ext + trees

parse :: String -> [Int]
parse = fmap (read . (: ""))

exteriorTrees :: [a] -> Int
exteriorTrees x = (length x - 1) * 4

--countTrees takes either +/-, current max, current x pos, list of trees -> list of x coords visible
countTrees :: (Int -> Int -> Int) -> Int -> Int -> [Int] -> [Int]
countTrees _ _ _ [] = []
countTrees _ _ _ [x] = [] --Stop at exterior
countTrees f m x (t : ts)
  | t > m = x : countTrees f t (f x 1) ts
  | otherwise = countTrees f m (f x 1) ts

part1 :: [[Int]] -> [(Int, Int)]
part1 ts = filter (\(x, y) -> not (x == 1 || y == 1 || x == length ts || y == length ts)) all
  where
    leftCount = concat $ zipWith (\xs y -> zip xs (repeat y)) (map (\t -> countTrees (+) (head t) 2 (tail t)) ts) [1 .. length ts]
    rightCount = concat $ zipWith (\xs y -> zip xs (repeat y)) (map (\t -> countTrees (-) (last t) (length ts - 1) ((tail . reverse) t)) ts) [1 .. length ts]
    t_ts = transpose ts
    downCount = concat $ zipWith (\xs y -> zip (repeat y) xs) (map (\t -> countTrees (+) (head t) 2 (tail t)) t_ts) [1 .. length t_ts]
    upCount = concat $ zipWith (\xs y -> zip (repeat y) xs) (map (\t -> countTrees (-) (last t) (length ts - 1) ((tail . reverse) t)) t_ts) [1 .. length ts]
    all = nub $ leftCount ++ rightCount ++ downCount ++ upCount
