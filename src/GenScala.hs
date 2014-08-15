module GenScala(genscala) where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad
import Data.Char


genscala :: IO [String]
genscala = do
  exprs <- sample' (arbitrary :: Gen Expr)
  return $ lines $ intercalate "\n" $ map show exprs


data Expr = Call String [Expr]
          | Op Expr String Expr
          | Match [(Unapply, Expr)] 
          | Statement String


instance Show Expr where
  show (Match matches) = "{\n" ++ concatMap (indent . show') matches ++ "\n}"
    where show' (un, ex) = "case " ++ show un ++ " => " ++ show ex
          indent s = unlines $ map (\x -> "  " ++ x) (lines s)
  show (Statement str) = str
  show (Call fname exprs) = fname ++ "(" ++ body ++ ")"
    where body = intercalate ", " . map show $ exprs
  show (Op ex1 op ex2) = show ex1 ++ " " ++ op ++ " " ++ show ex2


data Unapply = Unapply String
             | UnapplyClass String [Unapply] deriving (Eq)


instance Show Unapply where
  show (Unapply str) = str
  show (UnapplyClass str xs) = str ++ "(" ++ body ++ ")" 
    where body = intercalate ", " . map show $ xs



--todo: add comments and comment blocks as top-level constructs. Only ever need to be on own line, don't need to contain more than a token char or two
{- ex:

/**
 * ab
 */

// ab

/* ab */

-}

--todo: rename, make this OR .-sep method names, various literals. Key is ' and " chars, but can also have numbers
ngram :: Gen String
ngram = oneof [expr, literal]
  where literal = fmap (\x -> "\"" ++ x ++ "\"") word
        expr  = fmap (intercalate ".") parts
        parts = shortlist' $ oneof [word, typed]
        typed = do
           a <- word
           b <- upper
           return (a ++ "[" ++ b ++ "]")
        word  :: Gen String
        word  = oneof [lower, upper]
        lower = elements common
        upper = elements $ fmap (\(h:t) -> toUpper h:t) common



shortlist' :: Gen a -> Gen [a]
shortlist' g = oneof $ map sequence [[g],  
                                    [g, g], 
                                    [g, g, g]]

shortlist :: Arbitrary a => Gen [a]
shortlist = shortlist' arbitrary 


instance Arbitrary Unapply where
  arbitrary = frequency [(8,simple), (2,complex)]
    where 
     simple = liftM Unapply ngram
     complex = liftM2 UnapplyClass ngram shortlist
  

instance Arbitrary Expr where
  arbitrary = frequency [(3, statement), (1, op), (1, call), (1, match)]
    where
     match = liftM Match shortlist
     statement = liftM Statement ngram
     call = liftM2 Call ngram shortlist
     op = liftM3 Op arbitrary (elements operators) arbitrary

operators = ["+", "-", "*", "^", ">", "<", "<=", ">=", "|+|", ">|", ">>=", "|>|", "<*>",  "<**>", "|@|", ">=>", "&&", "||", "!", "?"]

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

