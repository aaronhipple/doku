module Loop (loop) where

import Control.Monad.State
  ( StateT,
    get,
    lift,
    modify,
  )
import Data.Char (digitToInt)
import Debug.Trace (traceShow)
import DokuState
  ( Direction (..),
    DokuState (..),
    check,
    clearViolations,
    mark,
    move,
    set,
  )
import Draw (drawGrid)
import UI.NCurses
import Prelude hiding (LT)

loop :: Window -> StateT DokuState Curses ()
loop w = do
  s <- get
  lift $ updateWindow w (drawGrid s)
  lift $ render
  ev <- lift $ getEvent w Nothing
  case ev of
    Nothing -> loop w
    Just e -> handleEvent e
      where
        handleEvent (EventSpecialKey KeyLeftArrow) = do
          modify $ move LT
          loop w
        handleEvent (EventSpecialKey KeyRightArrow) = do
          modify $ move RT
          loop w
        handleEvent (EventSpecialKey KeyUpArrow) = do
          modify $ move UP
          loop w
        handleEvent (EventSpecialKey KeyDownArrow) = do
          modify $ move DN
          loop w
        handleEvent (EventCharacter 'h') = do
          modify $ move LT
          loop w
        handleEvent (EventCharacter 'l') = do
          modify $ move RT
          loop w
        handleEvent (EventCharacter 'k') = do
          modify $ move UP
          loop w
        handleEvent (EventCharacter 'j') = do
          modify $ move DN
          loop w
        handleEvent (EventCharacter 'q') = lift (return ())
        handleEvent (EventCharacter 'Q') = lift (return ())
        handleEvent (EventCharacter c)
          | c `elem` "123456789" = do
            modify $ clearViolations
            modify $ set (Just $ digitToInt c)
            loop w
          | c `elem` "!@#$%^&*(" = do
            case translateShifts c of
              Nothing -> loop w
              Just n -> do
                modify $ clearViolations
                modify $ mark n
                loop w
          | c == '\DEL' = do
            modify $ set Nothing
            loop w
          | c == '\n' = do
            modify $ check
            loop w
        handleEvent (EventSpecialKey KeyDeleteCharacter) = do
          modify $ set Nothing
          loop w
        handleEvent e = loop w

translateShifts :: Char -> Maybe Int
translateShifts '!' = Just 1
translateShifts '@' = Just 2
translateShifts '#' = Just 3
translateShifts '$' = Just 4
translateShifts '%' = Just 5
translateShifts '^' = Just 6
translateShifts '&' = Just 7
translateShifts '*' = Just 8
translateShifts '(' = Just 9
translateShifts _ = Nothing
