module Main where

import Data.Char (isAlpha, isDigit)
import Data.List (transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import System.Environment (getArgs)

-- Parse file initially
main = do
  input <- readFile . head =<< getArgs
  let ls = lines input
      header = parseHeader . init $ takeWhile (/= "") ls
      body = map parseBody $ tail $ dropWhile (/= "") ls
  putStr $ show $ getSol $ applyMoves header body

-- Simplify
type Crates = M.Map Int String

type Command = Crates -> Crates

--get solution
getSol :: Crates -> String
getSol crt = head . snd <$> M.toList crt

-- xs our inital state, cs our list of commands
applyMoves :: Crates -> [Command] -> Crates
applyMoves = foldl (\x y -> y x)

-- function that the parser will use to create intermediate function
move :: Int -> Int -> Int -> Command
move amt from to crt =
  let top = take amt $ fromMaybe [] (M.lookup from crt)
   in M.adjust (top ++) to $ M.adjust (drop amt) from crt

--Parsing
parseBody :: String -> Command
parseBody xs = move x y z
  where
    [x, y, z] = map read $ filter (/= "") $ map (filter isDigit) $ words xs

parseHeader :: [String] -> M.Map Int String
parseHeader xs = M.fromList $ zip [1 ..] parsedCols
  where
    parsedCols = map concat . transpose $ map parseHeaderLine xs

parseHeaderLine :: String -> [String]
parseHeaderLine = map (filter isAlpha) . partition 4

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition x xs = take x xs : partition x (drop x xs)
