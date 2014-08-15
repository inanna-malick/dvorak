module Utils(shuffle, rnd_select, qwertyToDvorak)  where

import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.Array.IO


-- src: http://www.haskell.org/haskellwiki/Random_shuffle
 
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


-- taken from http://www.haskell.org/haskellwiki/index.php?title=99_questions/Solutions/23
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]


-- thank you mysterious pastebin stranger (code from http://pastebin.com/wYSrKB6z)
qwertyToDvorak:: Char -> Maybe Char
qwertyToDvorak c | c`elem` dvorak = Just $ qwertyDvorakMap Map.! c
                 | otherwise = Nothing
  where qwertyDvorakMap = Map.fromList (zip dvorak qwerty)


dvorak = " `1234567890[]',.pyfgcrl/=aoeuidhtns-\\;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+AOEUIDHTNS_|:QJKXBMWVZ"
qwerty = " `1234567890-=qwertyuiop[]asdfghjkl;'\\zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?"
