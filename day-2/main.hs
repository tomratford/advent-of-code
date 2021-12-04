--tuplify :: [a] -> (a,b)
tuplify :: [[Char]] -> ([Char], Int)
tuplify [x,y] = (x,read y :: Int)

getAim :: [([Char], Int)] -> Int -> [Int]
getAim [] n = [0]
getAim [x] n
  | xstr == "forward" = [n]
  | xstr == "down"    = [n + snd x]
  | xstr == "up"      = [n - snd x]
  where xstr = fst x
getAim (x:xs) n
  | xstr == "forward" = n : getAim xs n
  | xstr == "down"    = n + snd x : getAim xs (n + snd x)
  | xstr == "up"      = n - snd x : getAim xs (n - snd x)
  where xstr = fst x

dumbMult :: (([Char], Int), Int) -> Int
dumbMult ((x,y),z) = y * z

main = do
  ls <- readFile "input.txt"
  strs <- return (lines ls)
  let w = map words strs
      tup = map tuplify w
      fd = filter ((== "forward") . fst) tup
      dn = filter ((== "down") . fst) tup
      up = filter ((== "up") . fst) tup
      pt1 = sum (map snd fd) * (sum (map snd dn) - sum (map snd up))
  print pt1
  let aim = getAim tup 0
      zipaim = zip tup aim
      fdaim = filter ((== "forward") . fst . fst) zipaim
      pt2 = sum(map snd fd) * sum(map dumbMult fdaim)
  print pt2
