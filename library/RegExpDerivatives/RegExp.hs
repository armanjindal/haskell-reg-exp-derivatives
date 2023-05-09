-- | An example module.
module RegExpDerivatives.RegExp 
  (RegExp (..)
   , Parser
   , parse
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


type Parser = Parsec Void Text
  
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

--parseNot = undefined

parens :: Parser a -> Parser a
parens = between (single '(') (single ')')