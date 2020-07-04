module Draw (drawGrid) where

import Control.Monad (mapM_)
import Data.List (intercalate, (\\))
import DokuState
  ( CellState (..),
    DokuState (..),
  )
import UI.NCurses
  ( Attribute (..),
    Update,
    clearLine,
    cursorPosition,
    drawGlyph,
    drawLineH,
    drawLineV,
    drawString,
    glyphCornerLL,
    glyphCornerLR,
    glyphCornerUL,
    glyphCornerUR,
    glyphLineH,
    glyphLineV,
    glyphPlus,
    glyphTeeB,
    glyphTeeL,
    glyphTeeR,
    glyphTeeT,
    moveCursor,
    setAttribute,
    windowSize,
  )

marginTp = 1

marginLt = 1

paddingLt = 2

paddingRt = 0

paddingTp = 1

paddingBt = 0

cellPad = 3

boxSize = 3 + (2 * (cellPad)) + 1

drawGrid :: DokuState -> Update ()
drawGrid st = do
  drawBorders
  drawCells st
  drawViolations st
  (uncurry moveCursor) (posFrom (cursor st))

drawBorders :: Update ()
drawBorders = do
  moveCursor tp lt
  drawLineH (Just glyphLineH) sideLengthH
  drawLineV (Just glyphLineV) sideLengthV
  drawGlyph glyphCornerUL

  moveCursor tp outerRt
  drawLineV (Just glyphLineV) sideLengthV
  drawGlyph glyphCornerUR

  moveCursor outerBt lt
  drawLineH (Just glyphLineH) sideLengthH
  drawGlyph glyphCornerLL

  moveCursor outerBt outerRt
  drawGlyph glyphCornerLR

  moveCursor tp innerLt
  drawLineV (Just glyphLineV) sideLengthV
  drawGlyph glyphTeeT

  moveCursor outerBt innerLt
  drawGlyph glyphTeeB

  moveCursor tp innerRt
  drawLineV (Just glyphLineV) sideLengthV
  drawGlyph glyphTeeT

  moveCursor outerBt innerRt
  drawGlyph glyphTeeB

  moveCursor innerTp lt
  drawLineH (Just glyphLineH) sideLengthH
  drawGlyph glyphTeeL

  moveCursor innerTp outerRt
  drawGlyph glyphTeeR

  moveCursor innerTp innerLt
  drawGlyph glyphPlus

  moveCursor innerTp innerRt
  drawGlyph glyphPlus

  moveCursor innerBt lt
  drawLineH (Just glyphLineH) sideLengthH
  drawGlyph glyphTeeL

  moveCursor innerBt outerRt
  drawGlyph glyphTeeR

  moveCursor innerBt innerLt
  drawGlyph glyphPlus

  moveCursor innerBt innerRt
  drawGlyph glyphPlus
  where
    tp = marginTp
    lt = marginLt

    boxSizeH = (1 + paddingLt + boxSize + paddingRt)
    boxSizeV = (1 + paddingTp + boxSize + paddingBt)

    innerLt = marginLt + 1 * boxSizeH
    innerRt = marginLt + 2 * boxSizeH
    outerRt = marginLt + 3 * boxSizeH

    innerTp = marginTp + 1 * boxSizeV
    innerBt = marginTp + 2 * boxSizeV
    outerBt = marginTp + 3 * boxSizeV

    sideLengthH = 3 * boxSizeH
    sideLengthV = 3 * boxSizeV

drawCells :: DokuState -> Update ()
drawCells st =
  mapM_ drawCell cellStates
  where
    cellStates =
      [ (y, x, cell)
        | (y, row) <- zip [0 ..] (cells st),
          (x, cell) <- zip [0 ..] row
      ]

    drawCell (y, x, cell) = do
      case digit cell of
        Just n -> do
          clearAround (y, x)
          uncurry moveCursor $ posFrom (y, x)
          setAttribute AttributeBold True
          drawString (show n)
          setAttribute AttributeBold False
        Nothing -> do
          setAttribute AttributeDim True
          drawMarks (y, x, cell)
          setAttribute AttributeDim False

    drawMarks (y, x, cell) = do
      clearMarks (y, x) cell
      mapM_ (drawMark (y, x)) (marks cell)

    drawMark (y, x) n = do
      moveCursor (yBase + yOffset) (xBase + xOffset)
      drawString (show n)
      where
        (yBase, xBase) = posFrom (y, x)
        (yOffset, xOffset) = offset n

    clearMarks (y, x) cell = do
      let toClear = [1 .. 9] \\ marks cell
      mapM_ (clearMark (y, x)) toClear

    clearAround (y, x) = do
      let toClear = [1 .. 9] \\ [5]
      mapM_ (clearMark (y, x)) toClear

    clearMark (y, x) n = do
      moveCursor (yBase + yOffset) (xBase + xOffset)
      drawString " "
      where
        (yBase, xBase) = posFrom (y, x)
        (yOffset, xOffset) = offset n

    offset 1 = (-1, -1)
    offset 2 = (-1, 0)
    offset 3 = (-1, 1)
    offset 4 = (0, -1)
    offset 5 = (0, 0)
    offset 6 = (0, 1)
    offset 7 = (1, -1)
    offset 8 = (1, 0)
    offset 9 = (1, 1)

drawViolations :: DokuState -> Update ()
drawViolations st = do
  (height, width) <- windowSize
  let (y, x) = posFrom (10, 0)
  moveCursor y marginLt
  case violations st of
    Nothing -> clearLine
    Just [] -> drawString "OK!"
    Just vs -> drawString $ take (fromIntegral $ width - (marginLt * 2)) (intercalate ", " $ show <$> vs)

posFrom :: Integral a => (a, a) -> (Integer, Integer)
posFrom (y, x) = (toInteger $ yPos, toInteger $ xPos)
  where
    borderAndPaddingTp = 1 + paddingTp
    boxAndPaddingBt = boxSize + paddingBt
    (boxesTp, yInBox) = (fromIntegral y) `divMod` 3
    yPos =
      sum
        [ marginTp,
          (boxesTp + 1) * borderAndPaddingTp,
          boxesTp * boxAndPaddingBt,
          yInBox + (cellPad * yInBox)
        ]

    borderAndPaddingLt = 1 + paddingLt
    boxAndPaddingRt = boxSize + paddingRt
    (boxesLt, xInBox) = (fromIntegral x) `divMod` 3
    xPos =
      sum
        [ marginLt,
          (boxesLt + 1) * borderAndPaddingLt,
          boxesLt * boxAndPaddingRt,
          xInBox + (cellPad * xInBox)
        ]
