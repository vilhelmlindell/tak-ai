module Board (
    Board (..),
    Side (..),
    Stone (..),
    StoneType (..),
    StoneCount,
    Square,
    Direction,
    createBoard,
    placeStone,
    moveStack,
    normalStoneCount,
    capstoneCount,
    enemySide,
) where

import GHC.Arr
import Data.Bits
import Data.Word (Word64)

type Stack = [Stone]
type Square = Int
type StoneCount = (Int, Int)
type Bitboard = Word64

data Side = White | Black deriving (Show, Eq)

enemySide :: Side -> Side
enemySide White = Black
enemySide Black = White

data StoneType = Flat | Standing | Cap deriving (Show, Eq)

data Direction = North | South | West | East deriving (Show, Eq)

directionalOffset :: Int -> Direction -> Int
directionalOffset boardSize North = -boardSize
directionalOffset boardSize South = boardSize
directionalOffset _ West = -1
directionalOffset _ East = 1

data Stone = Stone
    { stoneSide :: Side
    , stoneType :: StoneType
    }
    deriving (Show, Eq)

initialStoneCounts :: Int -> (StoneCount, StoneCount)
initialStoneCounts size =
    let (normalStones, capstoneCount) = case size of
            3 -> (10, 0)
            4 -> (15, 0)
            5 -> (21, 1)
            6 -> (30, 1)
            7 -> (40, 2)
            8 -> (50, 2)
            _ -> error "Invalid board size"
     in ((normalStones, capstoneCount), (normalStones, capstoneCount))

data Board = Board
    { boardSize :: Int
    , stoneCounts :: (StoneCount, StoneCount)
    , squareStacks :: Array Int Stack
    , sideToMove :: Side
    }
    deriving (Show)

data BoardData = BoardData {
    blocked :: Bitboard,
    friendly :: Bitboard,

}

createBoard :: Int -> Board
createBoard size =
    let initialStacks = listArray (0, size * size - 1) (repeat [])
        initialCounts = initialStoneCounts size
     in Board
            { boardSize = size
            , stoneCounts = initialCounts
            , squareStacks = initialStacks
            , sideToMove = White
            , blockedBitboard = 0
            }

normalStoneCount :: Board -> Side -> Int
normalStoneCount board side =
    let (whiteCounts, blackCounts) = stoneCounts board
        (normalWhite, _) = whiteCounts
        (normalBlack, _) = blackCounts
     in case side of
            White -> normalWhite
            Black -> normalBlack

capstoneCount :: Board -> Side -> Int
capstoneCount board side =
    let (whiteCounts, blackCounts) = stoneCounts board
        (_, capstoneCountWhite) = whiteCounts
        (_, capstoneCountBlack) = blackCounts
     in case side of
            White -> capstoneCountWhite
            Black -> capstoneCountBlack

updateStoneCounts :: (StoneCount, StoneCount) -> Stone -> (StoneCount, StoneCount)
updateStoneCounts (whiteCounts, blackCounts) stone =
    let (normalWhite, capstoneCountWhite) = whiteCounts
        (normalBlack, capstoneCountBlack) = blackCounts
        (normalStones, capstoneCount) = case stoneSide stone of
            White -> (normalWhite, capstoneCountWhite)
            Black -> (normalBlack, capstoneCountBlack)
        newCounts = case stoneType stone of
            Flat -> (normalStones - 1, capstoneCount)
            Standing -> (normalStones - 1, capstoneCount)
            Cap -> (normalStones, capstoneCount - 1)
     in case stoneSide stone of
            White -> (newCounts, blackCounts)
            Black -> (whiteCounts, newCounts)

placeStone :: Board -> Square -> Stone -> Board
placeStone board@(Board{stoneCounts = counts, squareStacks = stacks, blockedBitboard = blocked}) square stone =
    let stack = [stone]
        newStacks = stacks // [(square, stack)]
        newCounts = updateStoneCounts counts stone
     in board{stoneCounts = newCounts, squareStacks = newStacks}

moveStack :: Board -> Square -> Direction -> Int -> Board
moveStack board@(Board{boardSize = size, squareStacks = stacks}) startSquare direction stonesToMove =
    let stack = squareStacks board ! startSquare
        (startRemoved, remainingStack) = splitAt stonesToMove stack
        endSquare = startSquare + directionalOffset size direction
        endStack = remainingStack ++ squareStacks board ! endSquare
        newStacks = stacks // [(startSquare, startRemoved), (endSquare, endStack)]
     in board{squareStacks = newStacks}
