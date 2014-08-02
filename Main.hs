module Main where

import UI.NCurses

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    let word = "test"
    cyan <- newColorID ColorCyan ColorBlack 3 
    white <- newColorID ColorWhite ColorBlack 4 
    updateWindow w $ do
        drawBox Nothing Nothing
    setCursorMode CursorInvisible 
    render
    demandWord w [] word white cyan
    closeWindow w

text :: [(String, ColorID)] -> Update ()
text xs = do sequence updates
             return ()
      where updates = map draw xs
            draw (s,c) = 
                do setColor c
                   drawString s
          


-- window -> already typed -> yet to type -> curse
demandWord :: Window -> String -> String -> ColorID -> ColorID -> Curses()
demandWord w typed totype white cyan = step typed totype
  where step typed (c:cs) = 
            do renderWord typed (c:cs)
               demandChar w c
               step (typed ++ [c]) cs

        step typed [] = 
            do renderWord typed []
               demandChar w 'q' 

        renderWord typed totype = 
            do (row, col) <- screenSize
               updateWindow w $ updateWord typed totype (row `div` 4, col `div` 2)
               render

        updateWord typed totype (row,col) = 
            do drawBox Nothing Nothing
               moveCursor row col
               text [(typed, cyan), (totype, white)]
               moveCursor 0 0



demandChar :: Window -> Char -> Curses()
demandChar w c = loop where
    loop = do
        ev <- getEvent w Nothing
        handleEvent ev

    handleEvent (Just (EventCharacter c')) 
         | c' == c    = return()
         | otherwise  = do flash
                           loop
    handleEvent _ = loop
                  
