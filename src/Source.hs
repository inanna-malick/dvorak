module Source where

import Text.CSV
import System.Random


loadwords :: IO [String]
loadwords = do
    Right csv <- parseCSVFromFile "grams/4grams-sorted.csv" --unsafe
    let extract (s:_:[]) = s --unsafe
    return $ map extract csv
