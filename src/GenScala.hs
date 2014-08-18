module GenScala(genscala) where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad
import Data.Char
import Control.Applicative

genscala :: IO [String]
genscala = do
  exprs <- (concat . repeat) <$> sample' (arbitrary :: Gen Expr)
  return $ lines $ intercalate "\n" $ map show exprs


data Expr = Call ScalaName [Expr]
          | Op Expr Operator Expr
          | Match [(Unapply, Expr)] 
          | Expr ScalaName

data Unapply = Unapply Literal
             | UnapplyClass ScalaName [Unapply]

-- a.B.c[A].d[F]
newtype ScalaName = ScalaName String

instance Show ScalaName where
  show (ScalaName s) = s

instance Arbitrary ScalaName where
        arbitrary = fmap (ScalaName . intercalate ".") parts
            where parts = shortlist' $ oneof [word, typed]
                  typed = do
                    a <- word
                    b <- upper
                    return (a ++ "[" ++ b ++ "]")
                  word  = oneof [lower, upper]
                  lower = elements ngrams
                  upper = elements $ fmap (\(h:t) -> toUpper h:t) ngrams

newtype Literal = Literal String

instance Arbitrary Literal where
        arbitrary = Literal <$> oneof [char, string]
          where char = elements $ map (wrap '\'' . (:[]) ) ['a'..'b']
                string = elements $ map (wrap '"') ngrams
                wrap c s = [c] ++ s ++ [c]

instance Show Literal where
  show (Literal s) = s

instance Show Expr where
  show (Match matches) = "{\n" ++ concatMap (indent . show') matches ++ "}"
    where show' (un, ex) = "case " ++ show un ++ " => " ++ show ex
          indent s = unlines $ map ("  "++) (lines s)
  show (Expr (ScalaName s)) = s
  show (Call (ScalaName fname) exprs) = fname ++ "(" ++ body ++ ")"
    where body = intercalate ", " . map show $ exprs
  show (Op ex1 (Operator op) ex2) = show ex1 ++ " " ++ op ++ " " ++ show ex2

instance Show Unapply where
  show (Unapply (Literal s)) = s
  show (UnapplyClass (ScalaName s) xs) = s ++ "(" ++ body ++ ")" 
    where body = intercalate ", " . map show $ xs

newtype Operator = Operator String

instance Show Operator where
        show (Operator s) = s

instance Arbitrary Operator where
        arbitrary = elements operators
            where operators = Operator <$> ["+", "-", "*", "^", ">", "<", "<=", ">=", "|+|", ">|", ">>=", "|>|", "<*>",  "<**>", "|@|", ">=>", "&&", "||", "!", "?"]

shortlist' :: Gen a -> Gen [a]
shortlist' g = frequency $ zip [1..] gens
    where gens = map sequence [[g,g,g],  
                              [g, g], 
                              [g]]


shortlist :: Arbitrary a => Gen [a]
shortlist = shortlist' arbitrary 


instance Arbitrary Unapply where
  arbitrary = frequency [(8,simple), (2,complex)]
    where 
     simple = liftM Unapply arbitrary
     complex = liftM2 UnapplyClass arbitrary shortlist
  
instance Arbitrary Expr where
  arbitrary = frequency [(3, expr), (1, op), (1, call), (1, match)]
    where
     match = liftM Match shortlist
     expr = liftM Expr arbitrary
     call = liftM2 Call arbitrary shortlist
     op = liftM3 Op arbitrary arbitrary arbitrary

--ngrams two-character strings
ngrams = ["th", "he", "an", "in", "er", "nd", "ou",
          "re", "ha", "ed", "on", "at", "hi", "to", 
          "en", "it", "ng", "as", "is", "st", "or", 
          "ar", "me", "es", "wa", "le", "te", "ey", 
          "ve", "se", "of", "ea", "ne", "ro", "nt", 
          "ut", "ho", "al", "no", "ow", "de", "be", 
          "om", "ad", "ti", "we", "co", "yo", "wh", 
          "el", "ee", "ri", "go", "ot", "li", "ur", 
          "fo", "sh", "ai", "ch", "so", "wi", "lo", 
          "et", "id", "la", "ma", "ld", "un", "im", 
          "us", "ce", "ay", "do", "ul", "ke", "ra", 
          "sa", "gh", "il", "ly", "ca", "ol", "ta", 
          "ac", "pe", "wo", "ge", "si", "ck", "ig", 
          "ic", "bu", "di", "tr", "ir", "rs", "bo"] 

