module Lottery (
    person, amount,
    buyTickets, drawTickets
) where

import Control.Monad.State

-- Internal types
type Lottery = [Person]

-- Exported types
newtype Person = Person String deriving (Eq, Show, Ord)
newtype Amount = Amount Int

person :: String -> Person
person = Person

amount :: Int -> Amount
amount = Amount

buyTickets :: Person -> State Lottery ()
buyTickets p = modify ((:) p)

drawTickets :: Amount -> State Lottery () -> Maybe [Person]
drawTickets (Amount a) s 
    | a <= 0 = Nothing
    | otherwise = do
        let s' = execState s []
        if length s' >= a
            then Just $ take a s'
            else Nothing
