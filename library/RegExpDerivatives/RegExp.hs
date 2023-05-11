-- | An example module.
module RegExpDerivatives.RegExp 
  (RegExp (..)
   , Parser
   , parse
   , deriv
   , justToDef
   , nullable
   , deriv
   , (~~)
  ) 
where


import Data.Text hiding(elem, foldl1)
import Text.Megaparsec hiding (parse, empty)
import Data.Void (Void)
import Prelude

-- Regular Expressions
data RegExp = EmptySet
    | Epsilon 
    | Ch Char 
    | Plus RegExp RegExp 
    | Concat RegExp RegExp 
    | Not RegExp 
    | Star RegExp 
    deriving (Eq, Ord)

-- TODO: Add Show NOT
instance Show RegExp where
  show = showConcat
    where
    showConcat (Concat a b) = show a ++ show b
    showConcat re = showPlus re

    showPlus (Plus a b) = show a ++ "+" ++ show b
    showPlus re = showStar re

    showStar (Star a) = show a ++ "*"
    showStar re = showNot re

    showNot (Not a) = "¬" ++ show a
    showNot re = showChar re

    showChar (Ch a) = [a]
    showChar Epsilon = "ε"
    showChar EmptySet = "∅"
    showChar re = "(" ++ showConcat re ++ ")"


type Parser = Parsec Void String
  
--parseNot = undefined
-- parseAnd = undefined

parse, parseChar, parseConcat, parsePlus, parseStar :: Parser RegExp

specialChars :: String
specialChars = "()ε∅\\+"

parse = parsePlus 
parsePlus = 
  foldl1 Plus <$> sepBy1 parseConcat (single '+')
parseConcat =
  foldl1 Concat <$> some parseStar
parseStar = 
  try (Star <$> parseChar <* single '*')
  <|> parseChar

parseChar = 
    try(Ch <$> satisfy (not . (`elem` specialChars)))
  <|> try(Ch <$> (single '\\' *> satisfy (`elem` specialChars)))
  <|> try(pure Epsilon <* single 'ε')
  <|> try(pure EmptySet <* single '∅')
  <|> parens parsePlus


parens :: Parser a -> Parser a
parens = between (single '(') (single ')')

-- check if the RegExp accepts the empty string
nullable :: RegExp -> Bool
nullable EmptySet = False
nullable Epsilon = True
nullable (Star a) = True
nullable (Ch a) = False
nullable (Concat a b) =  nullable a && nullable b
nullable (Plus a b) = nullable a || nullable b
-- nullable (And a b) = nullable a && nullable b
nullable (Not a) = not $ nullable a

deriv :: Char -> RegExp -> RegExp
deriv _ Epsilon = EmptySet
deriv a EmptySet = EmptySet
deriv a (Ch b)
  | a == b = Epsilon
  | a /= b = EmptySet
deriv a (Plus b c) = Plus (deriv a b) (deriv a c)
deriv a (Star b) = Concat (deriv a b) (Star b)
deriv a (Concat b c) = 
  if nullable b
  then Plus (Concat (deriv a b) c) (deriv a c)
  else Concat (deriv a b) c
-- deriv a (And b c), deriv a ()
-- Matching on the basis of algorithm 

(~~) :: RegExp -> String -> Bool
(~~) regex str = nullable (Prelude.foldr deriv regex str)

justToDef :: Maybe RegExp -> RegExp
justToDef (Just a) = a
justToDef Nothing = EmptySet