module Main where

import Data.List (nub)
import System.Environment (getArgs)

-- Parse file initially
main = do
  input <- readFile . head =<< getArgs
  putStr $ show $ part2 0 input

part1 :: Int -> String -> Int
part1 n xs = if 4 == length (nub subsection) then n+4 else part1 (n+1) $ tail xs
  where
    subsection = take 4 xs

part2 :: Int -> String -> Int
part2 n xs = if 14 == length (nub subsection) then n+14 else part2 (n+1) $ tail xs
  where
    subsection = take 14 xs
