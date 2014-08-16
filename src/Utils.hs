module Utils(qwertyToDvorak) where

import Data.Map (Map)
import qualified Data.Map as Map

-- thank you mysterious pastebin stranger (code from http://pastebin.com/wYSrKB6z)
qwertyToDvorak:: Char -> Maybe Char
qwertyToDvorak c = Map.lookup c qwertyDvorakMap
  where qwertyDvorakMap = Map.fromList (zip qwerty dvorak)
        dvorak = " `1234567890[]',.pyfgcrl/=aoeuidhtns-\\;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+AOEUIDHTNS_|:QJKXBMWVZ\DEL"
        qwerty = " `1234567890-=qwertyuiop[]asdfghjkl;'\\zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?\DEL"
