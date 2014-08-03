module Main where

import UI.NCurses
import Data.Map (Map)
import qualified Data.Map as Map
import Text.CSV
import System.Random

main :: IO ()
main =
 do words <- loadwords -- can load to many words to draw on screen
    topwords <- rnd_select words 5
    runCurses $ do
	setEcho False
	w <- defaultWindow
	cyan <- newColorID ColorCyan ColorBlack 3 
	white <- newColorID ColorWhite ColorBlack 4 
	setCursorMode CursorInvisible 
	(rows, cols) <- screenSize
	let config = Config {stdColor=white, emphasisColor=cyan, rows=rows, cols=cols}
	w' <- demandWord w topwords config
	closeWindow w'

-- taken from http://www.haskell.org/haskellwiki/index.php?title=99_questions/Solutions/23&action=edit
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]


loadwords :: IO [String]
loadwords = do
    Right csv <- parseCSVFromFile "grams/2grams-sorted.csv" --unsafe
    let extract (s:_:[]) = s --unsafe
    return $ map extract csv
     
    



text :: Line -> Update ()
text (Line xs) = 
    do sequence updates
       return ()
    where updates = map draw xs
          draw (s,c) = 
              do setColor c
                 drawString s
                 setColor defaultColorID

-- do all rendering work
renderWindow :: Window -> WData-> Curses()
renderWindow w (WData lines config) =  renderLines
  where startRow = rows config `div` 4
        
        renderLines = 
            do clearScreen w
               updateWindow w $ updateWord lines startRow
               render

        updateWord [] _ = return ()
        updateWord (line:xs) row =
            do drawBox Nothing Nothing
               moveCursor row (cols config `div` 2)
               text line
               moveCursor 0 0
               updateWord xs $ row + 1


clearScreen :: Window -> Curses ()
clearScreen w = 
    do (rows, cols) <- screenSize
       let blankline = (`replicate` ' ') . fromIntegral . (subtract 1) $ cols
       let clrLine row = do moveCursor row 0 >> drawString blankline
       updateWindow w $ sequence $ map clrLine [0..(rows-1)] 
       render


demandWord :: Window -> [String] -> Config -> Curses Window
demandWord w [] _ = return w
demandWord w (next:words) config = 
        do step [] next 
           demandWord w words config
  where step typed (c:cs) = 
            do renderWindow w wdata
               demandChar w $ qwertyToDvorak c
               step (typed ++ [c]) cs
          where wdata = WData wlines config
                currLine = Line [(typed, emphasisColor config), (c:cs, stdColor config)]
                wlines = currLine:rlines

        step typed [] = 
            do renderWindow w wdata
               demandChar w ' '
          where wdata = WData wlines config
                currLine = Line [(typed, emphasisColor config)]
                wlines = currLine:rlines

        rlines = map toline words
            where toline s = Line [(s, stdColor config)] 




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


-- thank you mysterious pastebin stranger (code from http://pastebin.com/wYSrKB6z)
dvorak = " `1234567890[]',.pyfgcrl/=aoeuidhtns-\\;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+AOEUIDHTNS_|:QJKXBMWVZ"
qwerty = " `1234567890-=qwertyuiop[]asdfghjkl;'\\zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?"
dvorakQwertyMap = Map.fromList (zip dvorak qwerty)
qwertyDvorakMap = Map.fromList (zip qwerty dvorak)

qwertyToDvorak:: Char -> Char
qwertyToDvorak c = dvorakQwertyMap Map.! c



