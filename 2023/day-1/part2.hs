import System.Environment (getArgs)
import Data.Char (isDigit)

main = do
  input <- readFile =<< fmap head getArgs
  putStr $ show $ sum $ map part1 (lines input)

convert :: String -> String
convert [] = []
convert ('o':'n':'e':a) = '1':convert ('e':a)
convert ('t':'w':'o':a) = '2':convert ('o':a)
convert ('t':'h':'r':'e':'e':a) = '3':convert ('e':a)
convert ('f':'o':'u':'r':a) = '4':convert ('r':a)
convert ('f':'i':'v':'e':a) = '5':convert ('e':a)
convert ('s':'i':'x':a) = '6':convert ('x':a)
convert ('s':'e':'v':'e':'n':a) = '7':convert ('n':a)
convert ('e':'i':'g':'h':'t':a) = '8':convert ('t':a)
convert ('n':'i':'n':'e':a) = '9':convert ('e':a)
convert (a:as) = a:convert as

part1 :: String -> Int
part1 xs = read $ head digits:[last digits]
  where digits = filter isDigit (convert xs)
