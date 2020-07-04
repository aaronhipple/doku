module Lib
  ( doku,
  )
where

import Control.Monad.State (execStateT)
import Data.List (replicate)
import DokuState (fromDigits)
import Loop (loop)
import UI.NCurses (defaultWindow, runCurses, setEcho)

doku :: IO ()
doku = runCurses $ do
  let s = aSudoku
  setEcho False
  w <- defaultWindow
  final <- execStateT (loop w) s
  return ()

aSudoku =
  fromDigits
    [ [0, 8, 0, 9, 0, 0, 2, 0, 5],
      [0, 5, 0, 3, 2, 0, 4, 8, 6],
      [0, 0, 0, 0, 8, 0, 0, 9, 0],
      [0, 0, 7, 0, 3, 9, 5, 0, 0],
      [0, 0, 0, 8, 0, 6, 0, 0, 0],
      [0, 0, 8, 1, 5, 0, 3, 0, 0],
      [0, 2, 0, 0, 4, 0, 0, 0, 0],
      [5, 9, 6, 0, 1, 8, 0, 3, 0],
      [7, 0, 3, 0, 0, 5, 0, 1, 0]
    ]
