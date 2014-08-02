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
    render
    demandWord w [] word white cyan


text :: [(String, ColorID)] -> Update ()
text xs = do sequence updates
             return ()
      where updates = map draw xs
            draw (s,c) = 
                do setColor c
                   drawString s
          


renderWord :: String -> String -> ColorID -> ColorID -> Update()
renderWord typed totype white cyan = 
    do drawBox Nothing Nothing
       moveCursor 3 10
       text [(typed, cyan), (totype, white)]

-- window -> already typed -> yet to type -> curse
demandWord :: Window -> String -> String -> ColorID -> ColorID -> Curses()
demandWord w typed (c:cs) white cyan =
    do updateWindow w $ renderWord typed (c:cs) white cyan 
       render
       demandChar w c
       demandWord w (typed ++ [c]) cs white cyan

demandWord w typed [] white cyan = 
    do updateWindow w $ renderWord typed [] white cyan
       render
       demandChar w 'q'





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
                  
