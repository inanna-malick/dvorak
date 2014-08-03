module Main where

import UI.NCurses
import Text.CSV
import Utils
 
main :: IO ()
main =
 do words <- loadwords
    topwords <- rnd_select words 5
    runCurses $ do
        setEcho False
	setCursorMode CursorInvisible 
	w <- defaultWindow
	cyan <- newColorID ColorCyan ColorBlack 3 
	white <- newColorID ColorWhite ColorBlack 4 
	(rows, cols) <- screenSize
	let config = Config {stdColor=white, emphasisColor=cyan, rows=rows, cols=cols}
	w' <- demandWord w topwords config
	closeWindow w'


loadwords :: IO [String]
loadwords = do
    Right csv <- parseCSVFromFile "grams/4grams-sorted.csv" --unsafe
    let extract (s:_:[]) = s --unsafe
    return $ map extract csv
     
text :: Line -> Update ()
text (Line xs) = sequence updates >> return ()
    where updates = draw `map` xs 
          draw (s,c) = setColor c >> drawString s >> setColor defaultColorID

renderWindow :: Window -> WData-> Curses()
renderWindow w (WData lines config) =  renderLines
  where startRow = rows config `div` 4
        renderLines = 
            do clearScreen w
               updateWindow w $ sequence $ map updateWord (zip lines [startRow..]) 
               render
        updateWord (line,row) =
            do drawBox Nothing Nothing
               moveCursor row (cols config `div` 2)
               text line
               moveCursor 0 0


clearScreen :: Window -> Curses ()
clearScreen w = 
    do (rows, cols) <- screenSize
       let blankline = (`replicate` ' ') . fromIntegral . (subtract 1) $ cols
       let clrLine row = do moveCursor row 0 >> drawString blankline
       updateWindow w $ sequence $ map clrLine [0..(rows-1)] 
       render


demandWord :: Window -> [String] -> Config -> Curses Window
demandWord w [] _ = return w
demandWord w (next:words) config = do step [] next >> demandWord w words config
  where step typed (c:cs) = 
              do renderWindow w wdata
                 demandChar w $ qwertyToDvorak c
                 step (typed ++ [c]) cs
           where wdata = WData wlines config
                 currLine = Line [(typed, emphasisColor config), (c:cs, stdColor config)]
                 wlines = currLine:rlines

        step typed [] = renderWindow w wdata >> demandChar w ' '
           where wdata = WData wlines config
                 currLine = Line [(typed, emphasisColor config)]
                 wlines = currLine:rlines

        rlines = map toline words
           where toline s = Line [(s, stdColor config)] 




demandChar :: Window -> Char -> Curses()
demandChar w c = loop where
    loop = getEvent w Nothing >>= handleEvent
    
    handleEvent (Just (EventCharacter c')) 
         | c' == c    = return()
	 | c' == '\ESC' = closeWindow w >> error "quit"
         | otherwise  = flash >> loop

    handleEvent _ = loop
                  
-- consider adding rows/columns
data Config = Config { stdColor :: ColorID
                     , emphasisColor :: ColorID
                     , rows :: Integer
                     , cols :: Integer
                     } deriving (Show)   


data Line = Line [(String, ColorID)]

data WData= WData [Line] Config


