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
  shuffled <- shuffle words
  runCurses . interpret . dvorak $ shuffled
  return ()



dvorak :: [String] -> Program ()
dvorak (first:rest) = step [] first
  where step typed totype@(c:cs) = do
          let current = Line [(Cyan, typed), (White, totype)]
          let next = map (Line . (:[]) ) $ zip (repeat Magenta) rest
          printlns $ current : next
          requestchar c
          step (typed ++ [c]) cs
        step typed [] = do
          println . Line $ [(Cyan, typed)]
          dvorak rest

dvorak [] = return ()
  

requestchar c = do
  c' <- getchar
  if (c' == qwertyToDvorak c)
    then return ()
    else signal WrongChar >> requestchar c

