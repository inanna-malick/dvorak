module Interaction where

import Control.Monad.Free

data ColorType = Cyan | White deriving (Show, Eq)
data Line = Line [(ColorType, String)] deriving (Show, Eq)

data SignalType = Success | WrongChar deriving (Show, Eq)

data Interaction next = 
    Print [Line] next
  | GetChar (Char -> next)
  | Signal SignalType next

instance Functor Interaction where
    fmap f (Print strs x) = Print strs (f x)
    fmap f (Signal sig x) = Signal sig (f x)
    fmap f (GetChar g) = GetChar (f . g)

type Program = Free Interaction


getchar :: Program Char
getchar = liftF (GetChar id)


printlns :: [Line] -> Program ()
printlns l = liftF (Print l ())

println :: Line -> Program ()
println l = liftF (Print [l] ())

-- todo: use println to implement
printstr :: String -> Program ()
printstr s = liftF (Print [Line [(White, s)]]  ())

signal :: SignalType -> Program ()
signal sig = liftF (Signal sig ())
