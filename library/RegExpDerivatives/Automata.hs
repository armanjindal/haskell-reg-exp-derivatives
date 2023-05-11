module RegExpDerivatives.Automata
where

{-
    - Define automata
    - Convert regex to automata
    - matching via automata
-}

-- newtype StateId = StateId {getStateId :: Int} deriving (Eq, Ord)

-- instance Show StateId where
--     show (StateId i) = "State_" ++ i
-- data State = 
--     State {stateId :: Int
--     , transitions :: () 
--     , accepting :: Bool
--     }

-- data Automaton = 
--     Automaton {
--         states :: Map StateId State,
--         startState :: StateId
--     }