-- | An example module.
module RegExp 
  (
    main
    , RegExp (..)
    ) where

-- | An example function.
main :: IO ()
main = putStrLn "Its alive"

-- Regular Expressions
data RegExp = EmptySet 
    | Epsilon 
    | Ch Char 
    | Concat RegExp RegExp 
    | Plus RegExp RegExp 
    | Not RegExp 
    | Star RegExp 
    deriving (Eq, Ord)

-- TODO: Add Show NOT
instance Show RegExp where
  show = showConcat
    where
    showConcat (Concat a b) = show a ++ "." ++ show b
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