module MoveGeneration (
    generatePlacements,
    generateMovements,
) where

import Board
import GHC.Arr

data Placement = Placement
    { placementSquare :: Square
    , stone :: Stone
    }
    deriving (Show)

data Movement = Movement
    { movementSquare :: Square
    , direction :: Direction
    , stonesToTake :: [Int]
    }
    deriving (Show)

generatePlacements :: Board -> [Placement]
generatePlacements board =
    let stacks = squareStacks board
     in foldl
            ( \acc square ->
                if null $ stacks ! square
                    then squarePlacements square (sideToMove board) ++ acc
                    else acc
            )
            []
            [0 .. (length stacks - 1)]

squarePlacements :: Square -> Side -> [Placement]
squarePlacements square side =
    let stones = map (\stoneType -> Stone{stoneType = stoneType, stoneSide = side}) [Flat, Standing, Cap]
     in map (\stone -> Placement{placementSquare = square, stone = stone}) stones

generateMovements :: Board -> [Movement]
generateMovements board = 

squareMovements :: Board -> Square -> Direction
squareMovements board square direction = 

possibleStackDrops :: Board -> Square -> Direction -> [[Int]]
possibleStackDrops board = [0..(min stackSize boardSize)]

