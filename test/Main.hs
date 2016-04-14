{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Refined
import Data.List
import System.Random
import qualified Data.Text as T

import Lottery

draw :: Spec
draw = do
    describe "drawTickets" $ do
        context "with x bought tickets" $ do
            it "draws exactly x winner when x tickets are drawn" $ property $
                \ps -> do
                    let len = length ps
                    let res = runLottery (mkStdGen 0) $ do
                            mapM_ buyTicket (ps :: [Person])
                            case refine len of
                                Right x -> drawTickets (Amount x)
                                Left _  -> error "cannot happen as length is always >= 0"
                    case len of
                        0 -> res `shouldBe` (Right [])
                        _ -> (sort <$> res) `shouldBe` (Right $ sort ps)
            it "returns Left with error message when drawing x+1 tickets" $ property $
                \ps -> do
                    let len = length ps
                    let res = runLottery (mkStdGen 0) $ do
                            mapM_ buyTicket (ps :: [Person])
                            case refine (len+1) of
                                Right x -> drawTickets (Amount x)
                                Left _  -> error "cannot happen as length is always >= 0"
                    res `shouldBe` (Left $ Error $ T.pack "Cannot draw more tickets than there are in the Lottery")
        it "returns no winners when 0 tickets are drawn" $ property $ do
            \ps -> do
                let len = length ps
                let res = runLottery (mkStdGen 0) $ do
                        mapM_ buyTicket (ps :: [Person])
                        drawTickets (Amount $$(refineTH 0))
                res `shouldBe` (Right [])
        it "returns different tickets with different seeds" $ do
            let ps = [Person $ T.pack "a", Person $ T.pack "b", Person $ T.pack "c"]
            let res1 = runLottery (mkStdGen 0) $ do
                        mapM_ buyTicket ps
                        drawTickets (Amount $$(refineTH 2))
            let res2 = runLottery (mkStdGen 4) $ do
                        mapM_ buyTicket ps
                        drawTickets (Amount $$(refineTH 2))
            res1 `shouldNotBe` res2

lottery :: Spec
lottery = describe "lottery" draw

main :: IO ()
main = hspec lottery
