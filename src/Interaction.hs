module Interaction where

import Control.Monad.Free

data ColorType = Cyan | White deriving (Show, Eq)
data Line = Line [(ColorType, String)] deriving (Show, Eq)

-- data Txt = Txt ColorType String | CR deriving (Show, Eq)

data SignalType = Success | WrongChar deriving (Show, Eq)

data Interaction next = 
    PrintLine Line next -- print a string to a 
  | GetChar (Char -> next)  -- get next Char
  | Signal SignalType next -- signal something

instance Functor Interaction where
    fmap f (PrintLine str x) = PrintLine str (f x)
    fmap f (Signal sig x) = Signal sig (f x)
    fmap f (GetChar g) = GetChar (f . g)

type Program = Free Interaction


getchar :: Program Char
getchar = liftF (GetChar id)

println :: Line -> Program ()
println l = liftF (PrintLine l ())

-- todo: use println to implement
printstr :: String -> Program ()
printstr s = liftF (PrintLine (Line [(White, s)])  ())

signal :: SignalType -> Program ()
signal sig = liftF (Signal sig ())
