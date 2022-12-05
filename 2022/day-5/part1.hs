module Main where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

-- Parse file initially
main = do
  input <- readFile . head =<< getArgs
  let ls = lines input
      header = takeWhile (/= "") ls
      body = tail $ dropWhile (/= "") ls
  putStr $ show header
  putStr $ show body

-- Simplify
type Crates = M.Map Int String

type Command = Crates -> Crates

-- xs our inital state, cs our list of commands
applyMoves :: Crates -> [Command] -> Crates
applyMoves = foldl (\x y -> y x)

-- function that the parser will use to create intermediate function
move :: Int -> Int -> Int -> Command
move amt from to crt =
  let top = take amt $ safeLookup from crt
   in M.adjust (top ++) to $ M.adjust (drop amt) from crt

safeLookup :: Int -> Crates -> String
safeLookup from crate = fromMaybe [] (M.lookup from crate)