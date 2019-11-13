import Options.Applicative
import UseCases.ReadLottery
import Models.Lottery

newtype Options = Options
    { optCommand :: Command
    }
parseOptions :: Parser Options
parseOptions = Options
                <$> hsubparser
                    ( command "show" (info show (progDesc "Show details of the lottery"))
                   <> command "buy" (info buy (progDesc "Buy a ticket in the lottery"))
                    )
                    where show = Show <$> parseShowOptions
                          buy = Buy <$> parseBuyOptions

data Command
    = Show ShowOptions
    | Buy BuyOptions

data ShowOptions = ShowOptions
parseShowOptions :: Parser ShowOptions
parseShowOptions = pure ShowOptions

data BuyOptions = BuyOptions
    { name :: String
    }
parseBuyOptions :: Parser BuyOptions
parseBuyOptions = BuyOptions <$> strArgument
                                    ( metavar "NAME"
                                   <> help "The name of the person who will buy a ticket"
                                    )

main :: IO ()
main = do
    (Options optCommand) <- execParser optionsParserInfo
    case optCommand of
        Show _ -> print getLottery
        Buy (BuyOptions name) -> print $ buyTicket (TicketHolder name)

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (parseOptions <**> helper)
    (  fullDesc
    <> progDesc "Manage your lottery"
    <> header "A lottery management tool"
    )
