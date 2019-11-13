{-# LANGUAGE TemplateHaskell #-}

module Models.Lottery where

import qualified Data.Map.Strict as Map
import Refined
import System.Random
import Data.Either.Extra

newtype Amount = Amount (Refined NonNegative Int) deriving (Eq, Show, Ord)

data TicketHolder = TicketHolder
    { name :: String
    } deriving (Show, Ord, Eq)

type LotteryName = String
data Lottery = Lottery
    { lotteryTickets :: Map.Map TicketHolder Amount
    , lotteryName    :: LotteryName
    } deriving (Show)

new :: LotteryName -> Lottery
new = Lottery Map.empty

add :: Lottery -> TicketHolder -> Lottery
add lottery@(Lottery oldTickets _) t = lottery
    { lotteryTickets = Map.insertWith addAmounts t (Amount $$(refineTH 1)) oldTickets
    }
    where addAmounts :: Amount -> Amount -> Amount
          -- Skip error handling because a positive amount added to another positive amount cannot be negative
          addAmounts (Amount a) (Amount b) = Amount . fromRight' . refine $ (unrefine a) + (unrefine b)
