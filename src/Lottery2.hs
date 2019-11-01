module Lottery2 where

import qualified Data.Map.Strict as Map
import Refined
import System.Random

newtype Amount = Amount (Refined NonNegative Int) deriving (Eq, Show, Ord)

type Name = String

data (Ord a, RandomGen g) => Lottery a g = Lottery
    { lotteryTickets :: Map.Map a Amount
    , lotteryRng     :: g
    , lotteryName    :: Name
    } deriving (Show)

new :: (Ord a, RandomGen g) => g -> Name -> Lottery a g
new = Lottery Map.empty
