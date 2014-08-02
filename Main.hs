module Main where

import UI.NCurses

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    let word = "test"
    cyan <- newColorID ColorCyan ColorBlack 3 
    white <- newColorID ColorWhite ColorBlack 4 
    setCursorMode CursorInvisible 
    (rows, cols) <- screenSize
    let config = Config {stdColor=white, emphasisColor=cyan, rows=rows, cols=cols}
    demandWord w [] word config
    closeWindow w

text :: Line -> Update ()
text (Line xs) = do sequence updates
                    return ()
       where updates = map draw xs
             draw (s,c) = 
                do setColor c
                   drawString s


-- do all rendering work
renderWindow :: Window -> WData-> Curses()
renderWindow w (WData lines config) =  return ()
  where renderWord typed totype = 
               do updateWindow w $ updateWord typed totype 
                  render

        updateWord typed totype =
            do drawBox Nothing Nothing
               moveCursor (rows config `div` 4) (cols config `div` 2)
               text $ Line [(typed, emphasisColor config), (totype, stdColor config)]
               moveCursor 0 0





demandWord :: Window -> String -> String -> Config -> Curses()
demandWord w typed totype config = step typed totype
  where step typed (c:cs) = 
            do renderWord typed (c:cs)
               demandChar w c
               step (typed ++ [c]) cs

        step typed [] = 
            do renderWord typed []
               demandChar w 'q' 

        renderWord typed totype = 
               do updateWindow w $ updateWord typed totype 
                  render

        updateWord typed totype =
            do drawBox Nothing Nothing
               moveCursor (rows config `div` 4) (cols config `div` 2)
               text $ Line [(typed, emphasisColor config), (totype, stdColor config)]
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
                  
-- consider adding rows/columns
data Config = Config { stdColor :: ColorID
                     , emphasisColor :: ColorID
                     , rows :: Integer
                     , cols :: Integer
                     } deriving (Show)   


data Line = Line [(String, ColorID)]

data WData= WData [Line] Config




