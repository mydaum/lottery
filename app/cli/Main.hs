{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import UseCases.ReadLottery

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
        Show _ -> print getLottery

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
