--Forewarning. This is some really scuffed code.
--It did not go the way I expected. I consistently submit entries into getBitVal (ln 70) which are not intended to be called.
--But it works so hey ho.

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (elemIndices)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Char (digitToInt)
import Debug.Trace (trace)

type Type = String
type ID = String
data Bit = Literal String | Operator Int Int [SubPacket] deriving (Show)
type SubPacket = M.Map (ID,Type) Bit

main = do
  putStr "File: "
  userInput <- getLine --Get user input for file
  rawFile <- readFile userInput --Read file as a single string
  let parsed = concatMap hexToDec rawFile
      run = parseSubpacket parsed
  --Pt1
  print $ getVersionSum run
  --Pt2
  print $ getVersionCalc run

--Parser funcs
binToDec :: String -> Int
binToDec b = sum $ map (2^) $ elemIndices '1' $ reverse b

hexToDec :: Char -> String
hexToDec x
  | x == '0' = "0000"
  | x == '2' = "0010"
  | x == '3' = "0011"
  | x == '4' = "0100"
  | x == '1' = "0001"
  | x == '5' = "0101"
  | x == '6' = "0110"
  | x == '7' = "0111"
  | x == '8' = "1000"
  | x == '9' = "1001"
  | x == 'A' = "1010"
  | x == 'B' = "1011"
  | x == 'C' = "1100"
  | x == 'D' = "1101"
  | x == 'E' = "1110"
  | x == 'F' = "1111"
  | otherwise = ""

mSplit :: Int -> [a] -> [[a]]
mSplit n [] = []
mSplit n xs = take n xs : mSplit n (drop n xs)

--Bit type ops
getBitLen :: Bit -> Int
getBitLen (Literal x) = 6 + length x + (length x `div` 4)
getBitLen (Operator i n xs)
  | i == 0 = 6 + 1 + 15 + n --6 + 1 + 15 Inefficient but easier to understand
  | i == 1 = 6 + 1 + 11 + sum (map getBitLen (concatMap M.elems xs)) --Get the lengths of the n sub packets after

getBitType :: Bit -> String
getBitType (Literal _) = "Literal"
getBitType Operator {} = "Operator"

getBitOperator :: Bit -> [SubPacket]
getBitOperator (Operator _ _ xs) = xs
getBitOperator x = []

getBitVal :: Bit -> Int
getBitVal (Literal x) = binToDec x
getBitVal x = trace ("EXHAUSTIVE CHECK getBitVal: " ++ show x) 0 

--Gets literal string of digits & returns string of what was parsed
parseLiteral :: String -> Bit
parseLiteral lit = Literal (concatMap tail split)
  where allSplit = mSplit 5 lit --Unfortunately parses the whole string. So O(n^2) at worst?
        endSet = elemIndices '0' (map head allSplit)
        split = take (head endSet + 1) allSplit

parseOp :: String -> Bit
parseOp (i:str) = Operator (digitToInt i) getLen subPacket
  where iLen = if i == '0' then 15 else 11
        getLen = (binToDec . take iLen) str
        subPacket = if i == '0' then
                      parseSubpacket ((take getLen . drop iLen) str)
                    else
                      take getLen $ parseSubpacket (drop iLen str)
parseOp x = trace ("ERROR: EXHAUSTIVE PATTERN MATCHING : " ++ show x) Literal "00001"

--Create subpacket from String
parseSubpacket :: String -> [SubPacket]
parseSubpacket empty
  | length empty <= 6 = []
parseSubpacket str = M.singleton (id,typ) parseRemaining : parseSubpacket remainingStr
  where id = take 3 str
        typ = take 3 (drop 3 str)
        parseRemaining = if binToDec typ == 4
                          then parseLiteral $ drop 6 str
                         else parseOp $ drop 6 str
        remainingStr = drop (getBitLen parseRemaining) str

--Get version sum (pt1)
getVersionSum :: [SubPacket] -> Int
getVersionSum [] = 0
getVersionSum (x:xs) = xIdVal + xBitCheck + getVersionSum xs
  where xIdVal = sum $ map (binToDec . fst) $ M.keys x
        xBitCheck = getVersionSum (getBitOperator (head $ M.elems x))

--Pt2
getOperator :: (Num a1, Num a2, Ord a2, Eq a1) => a1 -> [a2] -> a2
getOperator x
  | x == 0 = sum
  | x == 1 = product
  | x == 2 = minimum
  | x == 3 = maximum
  | x == 5 = gtList
  | x == 6 = ltList
  | x == 7 = eqList
  | otherwise = head

gtList :: (Ord a, Num p) => [a] -> p
gtList x = if head x > last x then 1 else 0

ltList :: (Ord a, Num p) => [a] -> p
ltList x = if head x < last x then 1 else 0

eqList :: (Eq a, Num p) => [a] -> p
eqList x = if head x == last x then 1 else 0

getVersionCalc :: [SubPacket] -> [Int]
getVersionCalc [] = []
getVersionCalc (x:xs)
  | (getBitType . getBits) x == "Literal" = trace ("Literals: " ++ show (map (getBitVal . getBits) (x:xs))) (getBitVal . getBits) x : getVersionCalc xs
  | otherwise = trace ("Op num: " ++ show ((binToDec . snd . head . M.keys) x)) (getOperator . binToDec . snd . head . M.keys) x (getVersionCalc ((getBitOperator . getBits) x)) : getVersionCalc xs
    where getBits = head . M.elems