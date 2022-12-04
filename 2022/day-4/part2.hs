import System.Environment (getArgs)
import Data.List (intersect)
import Data.Char (isDigit)

main = do
  input <- readFile =<< fmap head getArgs
  putStr $ show $ length $ filter part2 $ (lines input)

parseNum :: String -> (Int, String)
parseNum xs = (read num, str)
  where num = takeWhile isDigit xs
        str = dropWhile isDigit xs

parseLine :: String -> [Int]
parseLine [] = []
parseLine xs = n : if null str then [] else parseLine (tail str)
  where (n,str) = parseNum xs

part2 :: String -> Bool
part2 xs = (g1 >= g3 && g2 <= g4) || (g2 >= g3 && g2 <= g4) || (g4 <= g2 && g4 >= g1)
  where [g1,g2,g3,g4] = parseLine xs
