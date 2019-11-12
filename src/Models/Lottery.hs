module Models.Lottery where

import qualified Data.Map.Strict as Map
import Refined
import System.Random

newtype Amount = Amount (Refined NonNegative Int) deriving (Eq, Show, Ord)

type Name = String
type TicketNumber = Integer
data TicketHolder = TicketHolder
    { name :: String
    } deriving Show

data Lottery = Lottery
    { lotteryTickets :: Map.Map TicketNumber TicketHolder
    , lotteryName    :: Name
    } deriving (Show)

new :: Name -> Lottery
new = Lottery Map.empty
