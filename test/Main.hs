module Main where

import Lottery
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.List

draw :: Spec
draw = do
    describe "drawTickets" $ do
        context "with x bought ticket" $ do
            it "draws exactly those tickets when x tickets are drawn" $ property $
                \ps -> do
                    let ps' = map person ps
                    let r   = drawTickets (amount $ length ps) $ do
                            mapM_ buyTickets ps'
                    if (length ps > 0)
                        then (sort <$> r) `shouldBe` (Just $ sort ps')
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
