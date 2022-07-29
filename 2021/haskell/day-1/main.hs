--PT.1 : Sum values greater than last
gtCount :: (Num a,Ord a) => [a] -> a
gtCount [x] = 0
gtCount (x:y:xs) = (if x < y then 1 else 0) + gtCount (y : xs)

--PT.2 : Sliding windows
sumThree :: (Num a) => [a] -> [a]
sumThree [x,y,z] = [x + y + z]
sumThree (x:y:z:xs) = (x + y + z) : sumThree (y : z : xs)

main = do
  --Read in file
  ls <- readFile "input.txt"
  --Return monad of lines ls. So reads everything to str?
  strs <- return (lines ls)
  --Change to numbers
  let nums = map (read::String->Integer) strs
  --Actual pt.1 bit
  let val = gtCount nums
  putStr "The answer to part 1 is " 
  print val
  putStrLn ""
  --pt.2 bit
  let threeNums = sumThree nums
  let valTwo = gtCount threeNums
  putStr "The answer to part 2 is "
  print valTwo
