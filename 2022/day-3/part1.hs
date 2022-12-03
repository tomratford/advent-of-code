import System.Environment (getArgs)
import Data.List (intersect)

main = do
  input <- readFile =<< fmap head getArgs
  putStr $ show $ sum $ map part1 (lines input)

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

score :: Char -> Int
score x
 | fromEnum x < 97 = fromEnum x - 65 + 27
 | otherwise = fromEnum x - 97 + 1

part1 :: String -> Int
part1 xs = uncurry (\x y -> score . head $ intersect x y) (halve xs)
