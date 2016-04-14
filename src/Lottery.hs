module Lottery (
    Error(..), Amount(..), Person(..), Seed(..),
    buyTicket, drawTickets, runLottery
) where

import Control.Monad.State.Lazy
import Refined
import Test.QuickCheck.Arbitrary
import System.Random
import qualified Data.Text as T
import qualified Data.Foldable as F

newtype Error = Error T.Text deriving (Eq, Show)
newtype Person = Person T.Text deriving (Eq, Show, Ord)
newtype Amount = Amount (Refined NonNegative Int) deriving (Eq, Show, Ord)
newtype Seed = Seed Int

instance Arbitrary Person where
    arbitrary = do
        s <- arbitrary
        return $ Person $ T.pack s

-- Internal types
type Lottery g = State (Internal g)
data Internal g = Internal {
      tickets :: [Person]
    , rng     :: g
    }


runLottery :: RandomGen g => g -> Lottery g a -> a
runLottery g s = evalState s $ Internal { tickets = [], rng = g }

buyTicket :: RandomGen g => Person -> Lottery g ()
buyTicket p = modify $ modifyTickets (p:)

drawTickets :: RandomGen g => Amount -> Lottery g (Either Error [Person])
drawTickets (Amount a)
    | wanted == 0 = return $ Right []
    | otherwise = do
        available <- availableTickets
        case available >= wanted of
            True -> do
                person <- takeAndRemoveRandomPerson
                case refine (wanted-1) of
                    Right x -> do
                        nextPerson <- drawTickets (Amount x)
                        case nextPerson of
                            Right rest -> return $ Right (person:rest)
                            Left l     -> return $ Left l
                    Left _ -> return $ Left $ Error $ T.pack refineErr
            False -> return $ Left $ Error $ T.pack fewTickErr
    where fewTickErr = "Cannot draw more tickets than there are in the Lottery"
          refineErr  = "Tried to convert negative to natural number"
          wanted     = unrefine a

modifyTickets :: RandomGen g => ([Person] -> [Person]) -> Internal g -> Internal g
modifyTickets f i = i { tickets = f $ tickets i }

modifyRng :: RandomGen g => (g -> g) -> Internal g -> Internal g
modifyRng f i@(Internal { rng = r }) = i { rng = (f r) }

availableTickets :: RandomGen g => Lottery g Int
availableTickets = gets $ F.length . tickets

takeAndRemoveRandomPerson :: RandomGen g => Lottery g Person
takeAndRemoveRandomPerson = do
    ran <- getRandomAndUpdateRng
    person <- getPersonAtIndex ran
    removeTicketAtIndex ran
    return person

getRandomAndUpdateRng :: RandomGen g => Lottery g Int
getRandomAndUpdateRng = do
    available <- availableTickets
    (ran, newGen) <- gets $ randomR (0, available-1) . rng
    newRng newGen
    return ran

getPersonAtIndex :: RandomGen g => Int -> Lottery g Person
getPersonAtIndex i = gets $ (!!i) . tickets

removeTicketAtIndex :: RandomGen g => Int -> Lottery g ()
removeTicketAtIndex i = do
    (h, t) <- gets $ splitAt i . tickets
    newTickets $ h ++ (tail t)

newTickets :: RandomGen g => [Person] -> Lottery g ()
newTickets ts = modify $ modifyTickets (\_ -> ts)

newRng :: RandomGen g => g -> Lottery g ()
newRng newGen = modify $ modifyRng (\_ -> newGen)
