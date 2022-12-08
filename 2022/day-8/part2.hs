module Main where

import Data.List (nub, transpose)
import System.Environment (getArgs)

-- Parse file initially
main = do
  input <- readFile . head =<< getArgs
  let parsed = (fmap parse . lines) input
      allcoords = [(x, y) | x <- [1 .. length parsed], y <- [1 .. length parsed]]
  putStr $ show $ part2 parsed allcoords

parse :: String -> [Int]
parse = fmap (read . (: ""))

exteriorTrees :: [a] -> Int
exteriorTrees x = (length x - 1) * 4

--countTrees takes either +/-, current max, current x pos, list of trees -> list of x coords visible
countTrees :: Int -> [Int] -> Int
countTrees _ [] = 0
countTrees m (t : ts)
  | t >= m = 1
  | otherwise = 1 + countTrees m ts

part2 :: [[Int]] -> [(Int, Int)] -> Int
part2 ts all = maximum coordCount
  where
    xsplits = map (\(x, y) -> splitAt (x - 1) (ts !! (y - 1))) all
    ysplits = map (\(x, y) -> splitAt (y - 1) (transpose ts !! (x - 1))) all
    coordCount = zipWith (\x y -> part2Count x * part2Count y) xsplits ysplits

part2Count :: ([Int], [Int]) -> Int
part2Count (xs, h : ys) = leftTrees * rightTrees
  where
    leftTrees = countTrees h (reverse xs)
    rightTrees = countTrees h ys
