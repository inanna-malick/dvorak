module GenScala where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad
import Data.Char

-- just prints, use from ghci for now
examples = do
  exprs <- sample' (arbitrary :: Gen Expr)
  return $ lines $ concat $ intersperse "\n" $ map show exprs
{-
visualization idea: handle indentation by moving nonactive lines backward, clipping at edge. that way only inline spaces need typing. 
also, be whitespace flexible in some cases ex ""+"" vs "" + ""
-}

data Expr = Tuple2 (Expr, Expr) 
          | Call String [Expr]
          | Op Expr String Expr
          | Match [(Unapply, Expr)] 
          | Statement String


instance Show Expr where
  show (Tuple2 (a, b)) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (Match matches) = "{\n" ++ concat (map (indent . show') matches) ++ "\n}"
    where show' (un, ex) = "case " ++ show un ++ " => " ++ show ex
          indent s = unlines $ map (\x -> "  " ++ x) (lines s)
  show (Statement str) = str
  show (Call fname exprs) = fname ++ "(" ++ body ++ ")"
    where body = concat . (intersperse ", ") . (map show) $ exprs
  show (Op ex1 op ex2) = (show ex1) ++ " " ++ op ++ " " ++ (show ex2)


data Unapply = Unapply String
             | UnapplyClass String [Unapply] deriving (Eq)


instance Show Unapply where
  show (Unapply str) = str
  show (UnapplyClass str xs) = str ++ "(" ++ body ++ ")" 
    where body = concat . (intersperse ", ") . (map show) $ xs


ngram :: Gen String
ngram = fmap (concat . (intersperse ".")) parts
  where parts = shortlist' $ oneof [word, typed]
        typed = do
           a <- word
           b <- upper
           return (a ++ "[" ++ b ++ "]")
        word  :: Gen String
        word  = oneof [lower, upper]
        lower = elements common
        upper = elements $ fmap (\(h:t) -> (toUpper h):t) common



shortlist' :: Gen a -> Gen [a]
shortlist' g = oneof $ map sequence [[g],  
                                    [g, g], 
                                    [g, g, g]]

shortlist :: Arbitrary a => Gen [a]
shortlist = shortlist' arbitrary 


instance Arbitrary Unapply where
  arbitrary = frequency [(8,simple), (2,complex)]
    where 
     simple = liftM Unapply $ ngram
     complex = liftM2 UnapplyClass ngram shortlist
  

instance Arbitrary Expr where
  arbitrary = frequency [(3, statement), (1, t2), (1, op), (1, call), (1, match)]
    where
     t2 = liftM Tuple2 arbitrary
     match = liftM Match shortlist
     statement = liftM Statement ngram
     call = liftM2 Call ngram shortlist
     op = liftM3 Op arbitrary (elements operators) arbitrary

operators = ["=", "+", "-", "*", "^", ">", "<", "<=", ">=", "|+|", ">|", ">>=", "|>|", "<*>",  "<**>", "|@|", ">=>"]

common = ["th", "he", "an", "in", "er", "nd", "ou",
          "re", "ha", "ed", "on", "at", "hi", "to", 
          "en", "it", "ng", "as", "is", "st", "or", 
          "ar", "me", "es", "wa", "le", "te", "ll", 
          "ve", "se", "of", "ea", "ne", "ro", "nt", 
          "ut", "ho", "al", "no", "ow", "de", "be", 
          "om", "ad", "ti", "we", "co", "yo", "wh", 
          "el", "ee", "ri", "oo", "ot", "li", "ur", 
          "fo", "sh", "ai", "ch", "so", "wi", "lo", 
          "et", "id", "la", "ma", "ld", "un", "im", 
          "us", "ce", "ay", "do", "ul", "ke", "ra", 
          "sa", "gh", "il", "ly", "ca", "ol", "ta", 
          "ac", "pe", "wo", "ge", "si", "ck", "ig", 
          "ic", "bu", "di", "tr", "ir", "rs", "bo", 
          "go", "ey"]

