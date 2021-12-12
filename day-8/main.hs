import Data.List (sortOn, (\\), sort, group, nub, elemIndices)
import Debug.Trace (trace)

--Generalised version of 'words'
splitAt'     :: (Char -> Bool) -> String -> [String]
splitAt' f s =  case dropWhile f s of
                      "" -> []
                      s' -> w : splitAt' f s''
                            where (w, s'') = break f s'

-- Pt2
--
--   000
--  1   2
--  1   2
--   333
--  4   5
--  4   5
--   666

-- zero  = 012456
-- one   = 25     
-- two   = 02346  
-- three = 02356 
-- four  = 1325   
-- five  = 01356
-- six   = 013456 
-- seven = 025    
-- eight = 0123456
-- nine  = 012356 

-- N | Freq    | Method to find
-- 0 | 8 times | seven \\ one
-- 1 | 6 times | unique freq
-- 2 | 8 times | (freq) \\ 0
-- 3 | 7 times | four \\ one \\ 1
-- 4 | 4 times | unique freq
-- 5 | 9 times | unique freq
-- 6 | 7 times | \\ all

getSegs :: ([String],[String]) -> [String]
getSegs (sortLen,sortFreq) = [zero,one,two,three,four,five,six]
  where four = nub (head sortFreq)
        one = nub (sortFreq !! 1)
        five = nub $ last sortFreq
        zero = sortLen !! 1 \\ head sortLen
        two = (nub . concat $ filter (\ x -> length x == 8) sortFreq) \\ zero
        three = ((sortLen !! 2) \\ head sortLen) \\ one
        six = "abcdefg" \\ (zero ++ one ++ two ++ three ++ four ++ five)

decode :: String -> [String] -> String
decode partCode key
  | len == 2 = "1"
  | len == 3 = "7"
  | len == 4 = "4"
  | len == 7 = "8"
    where len = length partCode
decode partCode key
  | partDecode == [0,1,2,4,5,6] = "0"
  | partDecode == [0,2,3,4,6] = "2"
  | partDecode == [0,2,3,5,6] = "3"
  | partDecode == [0,1,3,5,6] = "5"
  | partDecode == [0,1,3,4,5,6] = "6"
  | partDecode == [0,1,2,3,5,6] = "9"
    where partDecode = sort $ concatMap (\x -> elemIndices x (concat key)) partCode
decode partCode key = trace ("partDecode:" ++ show partDecode) "X"
    where partDecode = sort $ concatMap (\x -> elemIndices x (concat key)) partCode

main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  --Pt1
  let split = map (splitAt' (=='|')) $ lines rawFile
      sndPart = map (concatMap words . tail) split
      uniques = map (filter (\ x -> length x `elem` [2,3,4,7])) sndPart
  print (length $ concat uniques)
  --pt2
  let fstPart = map (sortOn length . (words . head)) split
      fstFreq = map (sortOn length . group . sort . concat) fstPart
      fstSegs = zipWith (curry getSegs) fstPart fstFreq
      strCnts = zipWith (\x y -> concatMap (`decode` x) y) fstSegs sndPart
  print (head fstSegs)
  print (head sndPart)
  print (sum $ map read strCnts :: Int)