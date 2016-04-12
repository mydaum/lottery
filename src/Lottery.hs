module Lottery (
    Error(..), Amount(..), Person(..), Seed(..),
    buyTicket, drawTickets, runLottery
) where

import Control.Monad.State.Lazy
import Refined
import Test.QuickCheck.Arbitrary
import System.Random
import qualified Data.Text as T
import qualified Data.List as L
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
type Lottery = State Internal
data Internal = Internal
    {
      tickets :: [Person]
    , rng     :: StdGen
    }


runLottery :: Seed -> Lottery a -> a
runLottery (Seed n) s = evalState s $ Internal { tickets = [], rng = mkStdGen n }

buyTicket :: Person -> Lottery ()
buyTicket p = modify $ modifyTickets (p:)

drawTickets :: Amount -> Lottery (Either Error [Person])
drawTickets (Amount a)
    | wanted == 0 = return $ Right []
    | otherwise = do
        available <- availableTickets
        case available >= wanted of
            True -> do
                person <- takeAndRemoveRandomPerson
                case refine (wanted-1) of
                    Right x -> do
                        next <- drawTickets (Amount x)
                        case next of
                            Right rest -> return $ Right (person:rest)
                            Left l     -> return $ Left l
                    Left _ -> return $ Left $ Error $ T.pack refineErr
            False -> return $ Left $ Error $ T.pack fewTickErr
    where fewTickErr = "Cannot draw more tickets than there are in the Lottery"
          refineErr  = "Tried to convert negative to natural number"
          wanted     = unrefine a

modifyTickets :: ([Person] -> [Person]) -> Internal -> Internal
modifyTickets f i = i { tickets = f $ tickets i }

modifyRng :: (StdGen -> StdGen) -> Internal -> Internal
modifyRng f i = i { rng = f $ rng i }

availableTickets :: Lottery Int
availableTickets = gets $ F.length . tickets

takeAndRemoveRandomPerson :: Lottery Person
takeAndRemoveRandomPerson = do
    ran <- getRandomAndUpdateRng
    person <- getPersonAtIndex ran
    removeTicketAtIndex ran
    return person

getRandomAndUpdateRng :: Lottery Int
getRandomAndUpdateRng = do
    available <- availableTickets
    (ran, newGen) <- gets $ randomR (0, available-1) . rng
    newRng newGen
    return ran

getPersonAtIndex :: Int -> Lottery Person
getPersonAtIndex i = gets $ (!!i) . tickets

removeTicketAtIndex :: Int -> Lottery ()
removeTicketAtIndex i = do
    (h, t) <- gets $ splitAt i . tickets
    newTickets $ h ++ (tail t)

newTickets :: [Person] -> Lottery ()
newTickets ts = modify $ modifyTickets (\_ -> ts)

newRng :: StdGen -> Lottery ()
newRng newGen = modify $ modifyRng (\_ -> newGen)
