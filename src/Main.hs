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
  runCurses $ interpret $ dvorak shuffled
  return ()


simp = println (Line [(White, "foo")]) >> getchar

dvorak :: [String] -> Program ()
dvorak = requestwords 


requestwords (s:rest) = step [] s
  where step typed totype@(h:t) = do
          let current = Line [(Cyan, typed), (White, totype)]
          let next = map (\x -> Line [(Magenta, x)]) rest
          printlns $ current : next
          requestchar h
          step (typed ++ [h]) t
        step typed [] = do
          println $ Line [(Cyan, typed)]
          requestwords rest

requestwords [] = return ()
  

requestchar c = do
  c' <- getchar
  if (c' == qwertyToDvorak c)
    then return ()
    else signal WrongChar >> requestchar c

