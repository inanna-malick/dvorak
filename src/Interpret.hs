module Interpret where

import Control.Monad.Free
import UI.NCurses
import Utils
import Interaction

interpret :: Program r -> Curses r
interpret prog = do 
    setEcho False
    setCursorMode CursorInvisible
    cyan <- newColorID ColorCyan ColorBlack 3 
    white <- newColorID ColorWhite ColorBlack 4
    let colorFor c | c == Cyan = cyan
                   | c == White = white
    w <- defaultWindow
    r <- interpret' colorFor w prog
    closeWindow w
    return r
  where 
  interpret' :: (ColorType -> ColorID) -> Window -> Program r -> Curses r
  interpret' _ _ (Pure r) = return r
  interpret' cf w (Free (Signal sig next)) = case (sig) of 
        (Success) -> flash >> interpret' cf w next
        (WrongChar) -> flash >> interpret' cf w next
  interpret' cf w (Free (PrintLine (Line xs) next)) = do -- this does not work
        clearScreen w
        (rows,cols) <- screenSize
        let row = rows `div` 4
        let col = cols `div` 2
        updateWindow w $ drawBox Nothing Nothing
        updateWindow w $ moveCursor row col
        updateWindow w $ updates 
        render
        interpret' cf w next
    where updates = sequence $ map draw xs
          draw (c, s) = do setColor (cf c) >> drawString s >> setColor defaultColorID
  interpret' cf w (Free (GetChar g)) = do
        c <- loop
        interpret' cf w (g c)
    where loop = getEvent w Nothing >>= handleEvent  
          handleEvent (Just (EventCharacter c)) = return c
          handleEvent _ = loop



clearScreen :: Window -> Curses ()
clearScreen w = 
    do (rows, cols) <- screenSize
       let blankline = (`replicate` ' ') . fromIntegral . (subtract 1) $ cols
       let clrLine row = moveCursor row 0 >> drawString blankline
       updateWindow w $ sequence $ map clrLine [0..(rows-1)] 
       render
