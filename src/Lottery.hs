module Lottery (
    Error(..), Amount(..), Person(..),
    buyTicket, drawTickets, runLottery
) where

import Control.Monad.State.Lazy
import Refined
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Foldable as F

newtype Error = Error T.Text deriving (Eq, Show)
newtype Person = Person T.Text deriving (Eq, Show, Ord)
newtype Amount = Amount (Refined NonNegative Int) deriving (Eq, Show, Ord)

-- Internal types
type Lottery = State [Person]

runLottery :: Lottery a -> a
runLottery s = evalState s []

buyTicket :: Person -> Lottery ()
buyTicket p = modify (p:)

drawTickets :: Amount -> Lottery (Either Error [Person])
drawTickets (Amount a)
    | am == 0   = return $ Right []
    | otherwise = do
        len <- gets F.length
        case len >= am of
            True -> do
                ps <- gets $ L.take am
                modify $ L.drop am
                return $ Right ps
            False -> return $ Left $ Error $ T.pack errStr
    where errStr = "Cannot draw more tickets than there are in the Lottery"
          am = unrefine a
