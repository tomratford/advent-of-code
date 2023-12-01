import System.Environment (getArgs)
import Data.Char (isDigit)

main = do
  input <- readFile =<< fmap head getArgs
  putStr $ show $ sum $ map part1 (lines input)

part1 :: String -> Int
part1 xs = read $ (head digits):[(head . reverse) digits]
  where digits = filter isDigit xs
