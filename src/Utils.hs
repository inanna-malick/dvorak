module Utils(rnd_select, qwertyToDvorak)  where

import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.Array.IO



-- taken from http://www.haskell.org/haskellwiki/index.php?title=99_questions/Solutions/23
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, length xs - 1) gen]


-- thank you mysterious pastebin stranger (code from http://pastebin.com/wYSrKB6z)
qwertyToDvorak:: Char -> Maybe Char
qwertyToDvorak c = Map.lookup c qwertyDvorakMap
  where qwertyDvorakMap = Map.fromList (zip dvorak qwerty)
        dvorak = " `1234567890[]',.pyfgcrl/=aoeuidhtns-\\;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+AOEUIDHTNS_|:QJKXBMWVZ"
        qwerty = " `1234567890-=qwertyuiop[]asdfghjkl;'\\zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?"
