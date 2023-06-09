-- | An example module.
module RegExpDerivatives.RegExp 
  (RegExp (..)
   , Parser
   , parse
   , deriv
   , parseDef
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

-- Constructors for weak notion of equivalence (4.1)
ch :: Char -> RegExp
ch c = Ch c

emptySet :: RegExp
emptySet = EmptySet

epsilon :: RegExp
epsilon = Epsilon

concat :: RegExp -> RegExp -> RegExp
concat Epsilon b = b
concat a Epsilon = a
concat EmptySet b = EmptySet
concat a EmptySet = EmptySet
concat a b = Concat a b

plus :: RegExp -> RegExp -> RegExp
plus a b = if a == b then a else Plus a b
plus EmptySet b = b
plus b EmptySet = b
plus Epsilon b = if nullable b then b else Plus Epsilon b
plus a Epsilon = if nullable a then a else Plus a Epsilon

and :: RegExp -> RegExp -> RegExp
and a b = if a == b then a else And a b
and EmptySet b = EmptySet
and a EmptySet = EmptySet
and (Not EmptySet) b = b

star :: RegExp -> RegExp
star Epsilon = Epsilon
star (Star a) = Star a
star EmptySet = Epsilon

not :: RegExp -> RegExp
not (Not a) = a

-- These weak notions of equivalence ensure that any finite RE has a finite num of derivates and therefore states 
instance Show RegExp where
  show = showConcat
    where
    showConcat (Concat a b) = show a ++ "." ++ show b
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
    try(Ch <$> satisfy (Prelude.not . (`elem` specialChars)))
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
nullable (Not a) = Prelude.not $ nullable a

deriv :: Char -> RegExp -> RegExp
deriv _ Epsilon = EmptySet
deriv a EmptySet = EmptySet
deriv a (Ch b)
  | a == b = Epsilon
  | a /= b = EmptySet
deriv a (Plus b c) = Plus (deriv a b) (deriv a c)
deriv a (Star b) = Concat (deriv a b) (Star b)
deriv a (Concat b c) = Plus (Concat (deriv a b) c) (if nullable b then (deriv a c) else EmptySet)
deriv a (Not b) = Not (deriv a b)
deriv a (And b c) = And (deriv a b) (deriv a c)
-- Matching on the basis of algorithm 

{- A more efficient string matching function which
successivley applies for some string s = a1 a2 ... an on a regex r
deriv n (deriv n-1 .. deriv a2 (deriv a1 r)))) and checks if the result is nullable
if so => the string is in the language of r
-}
(~~) :: RegExp -> String -> Bool
(~~) regex [] = nullable regex
(~~) regex (s:ss) = (deriv s regex) ~~ ss

justToDef :: Maybe RegExp -> RegExp
justToDef (Just a) = a
justToDef Nothing = EmptySet

parseDef :: String -> RegExp
parseDef str = if regex == Nothing then error "bad string" else justToDef regex
          where regex = Text.Megaparsec.parseMaybe parse(str)
-- (~:) :: RegExp -> [String -> DFA
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

