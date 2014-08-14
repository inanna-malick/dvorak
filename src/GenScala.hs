module GenScala where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad


-- just prints, use from ghci for now
examples = do
  exprs <- sample' (arbitrary :: Gen Expr)
  return $ lines $ concat $ show `map` exprs
{-
goal here is close enough to correct syntax to build dvorak muscle memory:
paren counts should match, nothing nonsensical, but may or may not parse.
variable names are bs


visualization idea: handle indentation by moving nonactive lines backward, clipping at edge. that way only inline spaces need typing. 
also, be whitespace flexible in some cases ex ""+"" vs "" + ""
-}

{-
newtype ShortString = ShortString String
instance Arbitrary ShortString where ...
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

-- it's impossible to draw on any IO-requiring data inside arbitrary instances, so just choose from all two-letter words
ngrams = do
  a <- ['a' .. 'z']
  b <- ['a' .. 'z']
  return [a, b]

ngram :: Gen String
ngram = elements ngrams

shortlist :: Arbitrary a => Gen [a]
shortlist = oneof $ map promote [[arbitrary], [arbitrary, arbitrary], [arbitrary,arbitrary]]


shortlistN n = oneof $ map promote arbs
  where arbs = map (\x -> take x (repeat arbitrary)) [1..n]  

instance Arbitrary Unapply where
  arbitrary = frequency [(8,simple), (2,complex)]
    where 
     simple = liftM Unapply $ ngram
     complex = liftM2 UnapplyClass ngram $ shortlistN 2
  

instance Arbitrary Expr where
  arbitrary = frequency [(3, statement), (1, t2), (1, op), (1, call), (1, match)]
    where
     t2 = liftM Tuple2 arbitrary
     match = liftM Match $ shortlistN 2
     statement = liftM Statement ngram
     call = liftM2 Call ngram $ shortlistN 2
     op = liftM3 Op arbitrary (elements operators) arbitrary

operators = ["=", "+", "-", "*", "^", ">", "<", "<=", ">=", "|+|", ">|", ">>=", "|>|", "<*>",  "<**>", "|@|", ">=>"]



