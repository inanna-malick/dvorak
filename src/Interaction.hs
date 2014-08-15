module Interaction where

import Control.Monad.Free
import UI.NCurses

data ColorType = Cyan | White | Magenta | Red deriving (Show, Eq)
data Line = Line [(ColorType, String)] deriving (Show, Eq)

data SignalType = Success | WrongChar deriving (Show, Eq)

data Interaction next = 
    Print [Line] next
  | GetEvent (Either Char Key -> next)
  | Signal SignalType next

instance Functor Interaction where
    fmap f (Print strs x) = Print strs (f x)
    fmap f (Signal sig x) = Signal sig (f x)
    fmap f (GetEvent g) = GetEvent (f . g)

type Program = Free Interaction

getchar :: Program Char
getchar = do
  ev <- getev
  either return (const getchar) ev

getkey :: Program Key
getkey = do
  ev <- getev
  either (const getkey) return ev

getev :: Program (Either Char Key)
getev = liftF (GetEvent id)

printlns :: [Line] -> Program ()
printlns l = liftF (Print l ())

println :: Line -> Program ()
println = printlns . (:[])

printstr :: String -> Program ()
printstr = println . txt

txt :: String -> Line
txt s = Line [(White, s)]

signal :: SignalType -> Program ()
signal sig = liftF (Signal sig ())
