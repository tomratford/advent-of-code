import Data.Char (digitToInt)
import Data.Map ((!), )
import qualified Data.Map as M
import Data.List (findIndex)

--Type decs
type Flashed = Bool
type Energy  = Int
type Octopus = (Energy, Flashed)
type Coordinate = (Int, Int)
type Cavern = M.Map Coordinate Octopus


--Read in and main loop
main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  let energy = map digitToInt (filter (/='\n') rawFile)
      octopuses = zip energy (repeat False)
      allCoords = [(x,y) | x <- [0..9], y <- [0..9]]
      cavern = M.fromList $ zip allCoords octopuses
      cycle = iterate oneLoop cavern
      fstFlashed = findIndex (\ x -> length x == 100) (allFlash cycle)
  --Pt1
  print $ scoreCalc (take 101 cycle)
  print fstFlashed

--PT1

--Loop:
--Inc 1 energy + reset flashed to False \
--Find those >9 energy.                 \
--Flash - set flashed to True            |
--Get neighbours of flashed & increment /
--find those >9 energy                  \
--Flash - set flashed to True           |
-- ..
--repeat until none with >9 energy

--Funcs needed:
--Increment all & reset
-- [/] allInc
--Increment specific
-- [/] spInc
--Get flashed octs & update so they are flashed
-- [/] M.Map flashOct
--Get flashed octs & find neighbours
--Increment neighbours (If not flashed?)

--Increment funcs
allInc :: Cavern -> Cavern
allInc = M.map (\(x,_) -> (x+1,False))

spInc' :: [Coordinate] -> Cavern -> Cavern
spInc' coords = M.mapWithKey inc
  where inc key (x,f) = if key `elem` coords && not f then
                          (x+1,f)
                        else
                          (x,f)

spInc :: [Coordinate] -> Cavern -> Cavern
spInc coords cav = newcav
  where newcav = foldl (flip (M.adjust (\(x,y) -> if not y then (x+1,y) else (x,y)))) cav coords

--Flash funcs
hasFlashed :: Octopus -> Flashed
hasFlashed = snd

flashOct :: Octopus -> Octopus
flashOct (x,f)
  | f         = (x,f)
  | otherwise = if x > 9 then (0,True) else (x,f)

--Excludes self
getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (x,y) = [(i,j) | i <-[x-1..x+1],
                                j <-[y-1..y+1],
                                (i,j) /= (x,y),
                                i >= 0, i < 10,
                                j >= 0, j < 10 ]


--Get flashed, get neighbours, inc
flashLoop :: Cavern -> Cavern
flashLoop cav = incNeighbours
  where gt9 = M.filter (\x -> fst x > 9 && (not . hasFlashed) x) cav
        flashed = M.mapWithKey (\x y -> if x `M.member` gt9 then
                                          flashOct y
                                        else
                                          y) cav
        neighbourCoords = concatMap getNeighbours (M.keys gt9)
        incNeighbours = spInc neighbourCoords flashed

--flash cycle loop
repeatFlashLoop :: Cavern -> Cavern
repeatFlashLoop cav
  | flashLooped == cav = cav
  | otherwise            = repeatFlashLoop flashLooped
    where flashLooped = flashLoop cav

--One whole loop
oneLoop :: Cavern -> Cavern
oneLoop cav = repeatFlashLoop (allInc cav)

--Score calc
scoreCalcIndiv :: Cavern -> Int
scoreCalcIndiv cav = length $ M.toList $ M.filter hasFlashed cav

scoreCalc :: [Cavern] -> Int
scoreCalc cavs = sum $ map scoreCalcIndiv cavs

--Find allflash
allFlash :: [Cavern] -> [Cavern]
allFlash = map (M.filter hasFlashed)