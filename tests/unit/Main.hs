module Main (main) where

import Test.HUnit
import Lottery (person, amount, buyTickets, drawTickets)
import System.Exit (exitFailure)
import Data.Set (fromList, isSubsetOf)

test1 :: Test
test1 = TestCase $ do 
    let p        = person "Tomas"
    let Just [t] = drawTickets (amount 1) $ buyTickets p (amount 1)
    p @=? t

test2 :: Test
test2 = TestCase $ do 
    let p      = [person "Tomas", person "Vivi"]
    let Just t = drawTickets (amount 2) $ do
        mapM_ (flip buyTickets (amount 1)) p
    assertBool "Both ticker holders weren't drawn" $ same p t
    where same l1 l2 = isSubsetOf (fromList l1) (fromList l2)

test3 :: Test
test3 = TestCase $ do
    let t = drawTickets (amount 1) $ do
        buyTickets (person "Tomas") (amount 0)
    Nothing @=? t

tests :: Test
tests = TestList [
                   TestLabel "Buy one/draw one should have one winner" test1
                 , TestLabel "Buy two/draw two should have both as winners" test2
                 , TestLabel "Buy zero/draw one should return Nothing" test3
                 ]

main :: IO ()
main = do
    count <- runTestTT tests
    if failures count > 0 
        then exitFailure
        else return ()

