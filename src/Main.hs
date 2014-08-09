module Main where

-- import Text.CSV
import Utils
import Interaction
import Control.Monad.Free
import UI.NCurses
import Interpret
import Source

main :: IO ()
main = do
  words <- loadwords
  topwords <- rnd_select words 5
  runCurses $ interpret $ dvorak topwords
  return ()


simp = println (Line [(White, "foo")]) >> getchar

dvorak :: [String] -> Program ()
dvorak (x:xs) = do
  requestword x 
  dvorak xs
dvorak [] = return ()


requestword s = step [] s
  where step typed totype@(h:t) = do
          println $ Line [(Cyan, typed), (White, totype)]
          requestchar h
          step (typed ++ [h]) t
        step typed [] = do
          println $ Line [(Cyan, typed)]
    
  

requestchar c = do
  c' <- getchar
  if (c' == qwertyToDvorak c)
    then return ()
    else signal WrongChar >> requestchar c

