module Main where

import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = do
    _ <- execParser opts
    putStrLn "Hello World"
    where
        opts = info
            (fullDesc
            <> progDesc "Manage your Lotteries"
            <> header "A Lotter management tool")
