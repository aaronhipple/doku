module DokuState
  ( DokuState (..),
    CellState (..),
    fromDigits,
    move,
    Direction (..),
    set,
    mark,
    check,
    clearViolations,
    Region (..),
    Violation (..),
  )
where

import Data.List (transpose)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Prelude hiding (LT)

data DokuState = DokuState
  { cells :: [[CellState]],
    cursor :: (Int, Int),
    violations :: Maybe [Violation]
  }

data CellState = CellState
  { digit :: Maybe Digit,
    marks :: [Digit]
  }

type Digit = Int

fromDigits :: [[Int]] -> DokuState
fromDigits digits =
  DokuState
    { cells = cells,
      cursor = (0, 0),
      violations = Nothing
    }
  where
    cells = (fmap . fmap) f digits
    f 0 = CellState Nothing []
    f n
      | 0 < n && 10 > n = CellState (Just n) []
      | otherwise = error "digit out of range"

data Direction = UP | DN | LT | RT

move :: Direction -> DokuState -> DokuState
move UP st = st {cursor = ((9 + y - 1) `rem` 9, x)} where (y, x) = cursor st
move DN st = st {cursor = ((9 + y + 1) `rem` 9, x)} where (y, x) = cursor st
move LT st = st {cursor = (y, (9 + x - 1) `rem` 9)} where (y, x) = cursor st
move RT st = st {cursor = (y, (9 + x + 1) `rem` 9)} where (y, x) = cursor st

check :: DokuState -> DokuState
check st = st {violations = Just $ checkCells (cells st)}

clearViolations :: DokuState -> DokuState
clearViolations st = st {violations = Nothing}

data Region
  = Row Int
  | Column Int
  | Box Int

instance Show Region where
  show (Row n) = "Row " ++ show (n + 1)
  show (Column n) = "Column " ++ show (n + 1)
  show (Box n) = "Box " ++ show (n + 1)

data Violation
  = MissingDigits Region
  | DuplicatesDigits Region

instance Show Violation where
  show (MissingDigits region) = "Missing digits in " ++ show region
  show (DuplicatesDigits region) = "Repeated digits in " ++ show region

checkCells :: [[CellState]] -> [Violation]
checkCells xs = rowViolations ++ colViolations ++ boxViolations
  where
    rows = xs
    cols = transpose xs
    boxs = makeBoxes xs

    rowViolations = zip [0 ..] rows >>= checkCells Row
    colViolations = zip [0 ..] cols >>= checkCells Column
    boxViolations = zip [0 ..] boxs >>= checkCells Box

    checkCells r (i, cells) = construct <$> (checkMissingDigits digits ++ checkDuplicatesDigits digits)
      where
        construct v = v $ r i
        digits = catMaybes $ digit <$> cells

    checkDuplicatesDigits digits
      | Map.size (Map.filter (> 1) counts) > 0 = [DuplicatesDigits]
      | otherwise = []
      where
        counts = Map.fromListWith (+) (zip digits (repeat 1))

    checkMissingDigits digits
      | Set.fromList digits == Set.fromList [1 .. 9] = []
      | otherwise = [MissingDigits]

makeBoxes :: [[a]] -> [[a]]
makeBoxes = map concat . split 3 . concat . transpose . map (split 3)

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs = chunk : split n rest
  where
    (chunk, rest) = splitAt n xs

set :: Maybe Int -> DokuState -> DokuState
set n = update $ \cell -> cell {digit = n}

mark :: Int -> DokuState -> DokuState
mark n = update f
  where
    f cell
      | n `elem` oldMarks = cell {marks = filter (/= n) oldMarks}
      | otherwise = cell {marks = (n : oldMarks)}
      where
        oldMarks = marks cell

update :: (CellState -> CellState) -> DokuState -> DokuState
update f st = st {cells = newCells}
  where
    newCells = preRows ++ (newRow : postRows)
    newRow = preCells ++ (newCell : postCells)
    newCell = f oldCell

    (preRows, (oldRow : postRows)) = splitAt y oldCells
    (preCells, (oldCell : postCells)) = splitAt x oldRow

    oldCells = cells st
    (y, x) = cursor st
