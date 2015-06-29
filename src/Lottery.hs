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

buyTickets :: Person -> Amount -> State Lottery ()
buyTickets p (Amount a) = modify ((replicate a p) ++)

drawTickets :: Amount -> State Lottery () -> Maybe [Person]
drawTickets (Amount a) s = do
    let s' = execState s []
    if length s' >= a
        then Just $ take a s'
        else Nothing
