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

import Data.Set as Set
import qualified Data.List as L 
import Data.Text hiding(elem, foldl1, length, replicate)
import Text.Megaparsec hiding (parse, empty, State)
import Data.Void (Void)
import Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Regular Expressions
data RegExp = EmptySet
    | Epsilon 
    | Ch Char 
    | Plus RegExp RegExp 
    | Concat RegExp RegExp 
    | Not RegExp 
    | Star RegExp 
    | And RegExp RegExp
    deriving (Eq, Ord)

instance Show RegExp where
  show = showConcat
    where
    showConcat (Concat a b) = show a ++ show b
    showConcat re = showPlus re

    showPlus (Plus a b) = show a ++ "+" ++ show b
    showPlus re = showStar re

    showStar (Star a) = show a ++ "*"
    showStar re = showNot re

    showNot (Not a) =  show a ++ "~"
    showNot re = showChar re

    showChar (Ch a) = [a]
    showChar Epsilon = "ε"
    showChar EmptySet = "∅"
    showChar re = "(" ++ showConcat re ++ ")"


type Parser = Parsec Void String
  

parse, parseAnd, parseNot, parseChar, parseConcat, parsePlus, parseStar :: Parser RegExp

specialChars :: String
specialChars = "()ε∅\\+~&"

parse = parsePlus 
parsePlus = 
  foldl1 Plus <$> sepBy1 parseAnd (single '+')
parseAnd =
  foldl1 And <$> sepBy1 parseConcat (single '&')
parseConcat =
  foldl1 Concat <$> some parseStar
parseStar = 
  try (Star <$> parseNot <* single '*')
  <|> parseNot

parseNot =
  try (Not <$> parseChar <* single '~')
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
nullable (And a b) = nullable a && nullable b
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
  if nullable b -- then we want to be able to take the deriv of the other c
  then Plus (Concat (deriv a b) c) (deriv a c)
  else Concat (deriv a b) c 
deriv a (And b c) = And (deriv a b) (deriv a c)
deriv a (Not b) = Not (deriv a b)
-- Matching on the basis of algorithm 

{- A more efficient string matching function which
successivley applies for some string s = a1 a2 ... an on a regex r
deriv n (deriv n-1 .. deriv a2 (deriv a1 r)))) and checks if the result is nullable
if so => the string is in the language of r
-}
(~~) :: RegExp -> String -> Bool
(~~) regex str = nullable (Prelude.foldr deriv regex str)

justToDef :: Maybe RegExp -> RegExp
justToDef (Just a) = a
justToDef Nothing = EmptySet

-- (~:) :: RegExp -> String -> DFA
-- (~:) regex str = nullable (Prelude.foldr deriv regex str)
-- Function to generate a DFA from a regular expression using the Brzozwski algorithm

-- type Symbol = Char
-- type Transition = Map.Map (RegExp, Symbol) RegExp

-- data DFA = DFA
--   { states :: [RegExp]
--   , initial :: RegExp
--   , final :: [RegExp]
--   , alphabet :: Set.Set Symbol
--   , transition :: Transition
--   }

-- mkDFA :: RegExp -> String -> DFA
-- mkDFA r alpha = DFA states initial final alphabet transitions
--   where
--     initial = r
--     (states, transitions) = explore (initial, Map.empty) [initial]
--     final = Prelude.filter nullable states
--     alphabet = Set.fromList alpha
    
--     explore :: (RegExp, Transition) -> [RegExp] -> ([RegExp], Transition)
--     explore (q, trans) [] = ([q], trans)
--     explore (q, trans) (s:ss)
--       | (q,s) `Map.member` trans = explore (q, trans) ss
--       | otherwise = explore (q, newTrans) (s':ss) -- add to Map
--       where
--         s' = deriveAll s
--         newTrans = Map.insert s (Map.fromList [(symbol, deriv symbol s') | symbol <- Set.toList alphabet]) trans

--     deriveAll :: RegExp -> RegExp
--     deriveAll s = Prelude.foldr (\symbol acc -> deriv symbol acc) s (Set.toList alphabet)

