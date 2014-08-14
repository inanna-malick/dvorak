module Main where

-- import Text.CSV
import Utils
import Interaction
import Control.Monad.Free
import UI.NCurses
import Interpret
import Source
import GenScala

main :: IO ()
main = do
  words <- examples
  runCurses . interpret . dvorak $ words
  return ()



dvorak :: [String] -> Program ()
dvorak (first:rest) = step [] [] first
  where step :: String -> String -> String -> Program ()
        step wrong@(_:ws) typed totype = do
          let current = Line [(Cyan, typed), (Red, reverse wrong), (White, drop (length wrong) totype)] --todo: use red background + white text instead of magenta
          let next = map (Line . (:[]) ) $ zip (repeat White) rest
          printlns $ current : next
          ev <- getev --todo: need to throw away garbage chars (aka anything not space, tab, backspace or in dvorak mapping)
          let ifleft '\DEL'          = step ws typed totype -- this seems to be the actual 'backspace'
              ifleft  c            = step (c:wrong) typed totype
              ifright KeyBackspace = step ws typed totype -- I guess this never happens...
              ifright _            = step wrong typed totype
          either ifleft ifright ev
          
        step [] typed totype@(c:cs) = do
          let current = Line [(Cyan, typed), (White, totype)]
          let next = map (Line . (:[]) ) $ zip (repeat Magenta) rest
          printlns $ current : next
          c' <- getchar
          if (c' == c) 
            then step [] (typed ++ [c]) cs
            else step [c'] typed totype

        step [] typed [] = do
          println . Line $ [(Cyan, typed)]
          dvorak rest

dvorak [] = return ()



requestchar c = do
  c' <- getchar
  if (c' == c)-- qwertyToDvorak c)
    then return ()
    else signal WrongChar >> requestchar c

