import Data.Char (digitToInt)

type Coord = (Int,Int)
data Octopus = Oct Int Coord deriving (Show)
type Cavern = (Int,[Octopus])

main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  let energy = map digitToInt (filter (/='\n') rawFile)
      allCoords = [(x,y) | x <- [0..9], y <- [0..9]]
      cavern = zipWith Oct energy allCoords
      simpStep = cavernStep allCoords
      simpLoop = (\ x -> foldl octCavLoop x [0..99])
      basicStep = iterate oneCycle (0,cavern) !! 2
  print $ basicStep

--Rewrite all this for the new data types

getEnergy :: Octopus -> Int
getEnergy (Oct energy _) = energy

getCoord :: Octopus -> Coord
getCoord (Oct _ coord) = coord

getOctopus :: Cavern -> Coord -> Octopus
getOctopus (i,octs) coord = head $ filter (\ x -> getCoord x == coord) octs

getNeighbours :: Coord -> [Coord]
getNeighbours (x,y) = [(x+i,x+j) | i <- [1,0,-1], j <- [1,0,-1],
                                   (x+i,y+j) /= (x,y),
                                   x+i >= 0, y+j >= 0,
                                   x+i < 10, y+j < 10]

--Step/Inc funcs
octStep :: Octopus -> Octopus
octStep (Oct energy c) = Oct (energy+1) c

cavernStep :: [Coord] -> Cavern -> Cavern
cavernStep coords (i, octs) = (i, map (\ x -> if getCoord x `elem` coords then octStep x else x) octs)

setZero ::  Octopus -> Cavern -> Cavern
setZero (Oct energy coord) (i, octs) = (if energyCheck == 0 then i+1 else i, map (\ x -> if getCoord x == coord then newOct else x) octs)
    where energyCheck = if energy > 9 then 0 else energy
          newOct = Oct energyCheck coord

--Funcs to check for flashes
octFlash :: Octopus -> [Coord]
octFlash (Oct energy coord)
  | energy > 9 = getNeighbours coord
  | otherwise  = []

octCavLoop :: Cavern -> Int -> Cavern
octCavLoop (i, octs) j = (setZero oct . cavernStep flash) (i,octs)
  where oct = octs !! j
        flash = octFlash oct

--2nd Attempt

cavernStep' :: [Coord] -> Cavern -> Cavern
cavernStep' coords (i, octs) = (i, map (\ x -> if getCoord x `elem` coords && getEnergy x >= 0 then octStep x else x) octs)

setZero' :: [Coord] -> Cavern -> Cavern
setZero' coords (i, octs) = (i + length coords, map (\ x -> if getCoord x `elem` coords then Oct (-1) (getCoord x) else x) octs)

getFlashers :: Cavern -> [Coord]
getFlashers (i, octs) = map getCoord $ filter ((>9) . getEnergy) octs

setFlashers :: Cavern -> Cavern
setFlashers x = setZero' (getFlashers x) x

getFlashNeighbours :: Cavern -> [Coord]
getFlashNeighbours = concatMap getNeighbours . getFlashers

incFlashNeighbours :: Cavern -> Cavern -> Cavern
incFlashNeighbours oldcav flashedcav = cavernStep' (getFlashNeighbours oldcav) flashedcav

calcFlashScore :: Cavern -> Cavern
calcFlashScore cav = newCav
  where flashedCav = setFlashers cav
        newCav = incFlashNeighbours cav flashedCav

flashCycle :: Cavern -> Cavern
flashCycle cav
  | fst (calcFlashScore cav) == fst cav = cav
  | otherwise                           = flashCycle cav

oneCycle :: Cavern -> Cavern
oneCycle = flashCycle . cavernStep' allCoords
  where allCoords = [(x,y) | x <- [0..9], y <- [0..9]]

        