import System.Environment (getArgs)
import Data.Char (isDigit)

main = do
  input <- readFile =<< fmap head getArgs
  putStr $ show $ sum $ map part1 (lines input)

convert :: String -> String
convert [] = []
convert ('o':'n':'e':a) = 'o':'1':convert ('e':a)
convert ('t':'w':'o':a) = 't':'2':convert ('o':a)
convert ('t':'h':'r':'e':'e':a) = 't':'3':convert ('e':a)
convert ('f':'o':'u':'r':a) = 'f':'4':convert ('r':a)
convert ('f':'i':'v':'e':a) = 'f':'5':convert ('e':a)
convert ('s':'i':'x':a) = 's':'6':convert ('x':a)
convert ('s':'e':'v':'e':'n':a) = 's':'7':convert ('n':a)
convert ('e':'i':'g':'h':'t':a) = 'e':'8':convert ('t':a)
convert ('n':'i':'n':'e':a) = 'n':'9':convert ('e':a)
convert (a:as) = a:convert as

part1 :: String -> Int
part1 xs = read $ head digits:[last digits]
  where digits = filter isDigit (convert xs)
