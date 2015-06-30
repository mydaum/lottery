module Main where

import Lottery 
import Test.Hspec
import Test.QuickCheck
import Control.Monad

draw :: Spec
draw = do
    describe "drawTickets" $ do
        context "with x bought ticket" $ do
            it "draws all those tickets when x tickets are drawn" $ property $ 
                \a -> do 
                    let p = person "Test"
                    let r = drawTickets (amount a) $ do
                        replicateM_ a $ buyTickets p
                    if (a > 0)
                        then r `shouldBe` (Just (replicate a p))
                        else r `shouldBe` Nothing
        it "returns Nothing when 0 tickets are drawn" $ do
            let r = drawTickets (amount 0) $ buyTickets $ person "Test" 
            r `shouldBe` Nothing
        it "returns Nothing when tickets drawn > ticket bought" $ property $
            \a -> do
                let r = drawTickets (amount (a+1)) $ do
                    replicateM_ a $ buyTickets $ person "Test"
                r `shouldBe` Nothing

lottery :: Spec
lottery = describe "lottery" draw

main :: IO ()
main = hspec lottery
