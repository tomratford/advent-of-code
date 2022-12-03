import System.Environment (getArgs)
import Data.List (intersect)

main = do
  input <- readFile =<< fmap head getArgs
  putStr $ (show . sum) $ map part2 $ partition 3 (lines input)

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

score :: Char -> Int
score x
 | fromEnum x < 97 = fromEnum x - 65 + 27
 | otherwise = fromEnum x - 97 + 1

part2 :: [String] -> Int
part2 xs = score $ head $ foldr1 intersect xs


