{-# LANGUAGE DeriveFunctor #-}
module Interaction where

import Control.Monad.Free

data ColorType = Cyan | White | Magenta | Red deriving (Show, Eq)
data Line = Line [(ColorType, String)] deriving (Show, Eq)

data SignalType = Success | WrongChar deriving (Show, Eq)

data Interaction next = 
    Print [Line] next
  | GetChar (Char -> next)
  | Signal SignalType next deriving (Functor)

type Program = Free Interaction

getchar :: Program Char
getchar = liftF (GetChar id)

printlns :: [Line] -> Program ()
printlns l = liftF (Print l ())

signal :: SignalType -> Program ()
signal sig = liftF (Signal sig ())
