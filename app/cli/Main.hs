{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import System.Random (getStdGen)
import Refined (refineTH)
import Data.Char (isDigit)
import Data.Text (pack)
import Control.Monad (forM_)

import Text.ParserCombinators.ReadP
import Options.Applicative
import System.IO
import Lottery

lotteryInfo = info helper
    (  fullDesc
    <> progDesc "Manage your lottery"
    <> header "A lottery management tool"
    )

main :: IO ()
main = do
    _ <- execParser lotteryInfo
    putStrLn "Hello World"
