{-# LANGUAGE RecordWildCards #-}
module Day3 where

import Data.Monoid
import Data.Foldable

-- Each cell at the spiral has a position
data Position = Pos Int Int deriving (Show)

stepsToSquareOne (Pos x y) = abs x + abs y

-- At each next step in the spiral, we can move Up, Down, Left or Right.
-- As it turns out, moves are just relative positions.
left = Pos (-1) 0
right = Pos 1 0
up = Pos 0 1
down = Pos 0 (-1)

-- We can generate the infinite list of moves to walk over the entire spiral
allSpiralMoves = rightAndUp 1
  where 
    rightAndUp n = replicate n right ++ replicate n up ++ leftAndDown (n+1)
    leftAndDown n = replicate n left ++ replicate n down ++ rightAndUp (n+1)

-- And relative positions are monoidial!
instance Monoid Position where
  mempty = Pos 0 0
  mappend (Pos x y) (Pos x' y') = Pos (x+x') (y+y')

-- Which makes the answer unfairly simple
answer n = stepsToSquareOne 
         $ mconcat
         $ take (n-1) allSpiralMoves



-- Part 2
data Cell = Cell {
    cellValue :: Int
  , cellPos :: Position
  } deriving (Show)

isAdjacentTo :: Position -> Position -> Bool
isAdjacentTo (Pos x y) (Pos x' y') = abs (x-x') <= 1 && abs (y-y') <= 1 

initialCell = Cell 1 (Pos 0 0)

answer' = 
  -- Find the first cell with value greater than 312051
  find ((> 312051) . cellValue) 
  -- Starting with only initialCell and all moves left
  $ go [initialCell] allSpiralMoves
  where
    -- Recursively build the list of cells, while reducing the list of moves.
    go cells (move:moves) = nextCell : go (nextCell : cells) moves
      where
        nextCell = Cell newValue newPos
        newValue = sum $ cellValue <$> adjacentCells
        newPos = move <> (cellPos $ head cells)
        adjacentCells = filter (isAdjacentTo newPos . cellPos) cells