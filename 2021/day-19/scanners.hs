module Scanners where

import Data.List (nub)

data Coord = C {
  _x :: Int,
  _y :: Int,
  _z :: Int
} deriving (Eq)
type Scanners = (Coord, [Coord])

instance Show Coord where
  show (C x1 x2 x3) = "(" ++ show x1 ++ ", " ++ show x2 ++ ", " ++ show x3 ++ ")"

instance Num Coord where
  C x1 x2 x3 + C y1 y2 y3 = C (x1 + y1) (x2 + y2) (x3 + y3)
  C x1 x2 x3 * C y1 y2 y3 = C (x1 * y1) (x2 * y2) (x3 * y3)
  abs (C x1 x2 x3) = C (abs x1) (abs x2) (abs x3) 
  signum (C x1 x2 x3) = let sign x
                              | x > 0 = 1
                              | x == 0 = 0
                              | otherwise = -1 in
                          C (sign x1) (sign x2) (sign x3)
  fromInteger x = let y = fromInteger x in C y y y
  C x1 x2 x3 - C y1 y2 y3 = C (x1 - y1) (x2 - y2) (x3 - y3)

rotations :: [Coord]
rotations = nub $ [c z | c <- ys,
                   z <- [1, -1]]
  where xs = [C x | x <- [1, -1]]
        ys = [c y | c <- xs,
                    y <- [1, -1]]
        
