{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Lottery2
import System.Random

newtype Options = Options
    { optCommand :: Command
    }

newtype Command
    = Show ShowOptions

data ShowOptions = ShowOptions

main :: IO ()
main = do
    (Options optCommand) <- execParser optionsParserInfo
    case optCommand of
        Show _ -> print predefinedLottery

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionParser <**> helper)
    (  fullDesc
    <> progDesc "Manage your lottery"
    <> header "A lottery management tool"
    )
    where optionParser = Options
            <$> subparser
                ( command "show" (info show (progDesc "Show details of the lottery"))
                )
                where show = Show <$> pure ShowOptions

predefinedLottery :: Lottery String StdGen
predefinedLottery = new (mkStdGen 0) "Test lottery"
