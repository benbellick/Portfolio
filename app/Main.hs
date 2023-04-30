{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Portfolio
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import Config

data Options = Options
  { optArgument :: Argument,
    optConfigPath :: FilePath
  }

data ConfigCommand = Show | Set
data Argument = Buy Double
              | Sell Double
              | Rebalance
              | Configure ConfigCommand

opts :: Parser Options
opts = Options <$> argumentParser <*> configPathParser

argumentParser :: Parser Argument
argumentParser = subparser
                 (  command "buy" (info buyCommand (progDesc "buy a specific value of target portfolio"))
                 <> command "sell" (info sellCommand (progDesc "sell a specific value of target portfolio"))
                 <> command "rebalance" (info rebalanceCommand (progDesc "rebalance portfolio"))
                 <> command "config" (info configCommand (progDesc "configure"))
                 )
buyCommand, sellCommand, rebalanceCommand, configCommand, configShowCommand, configSetCommand :: Parser Argument
buyCommand = Buy <$> argument auto (metavar "AMT")
sellCommand = Sell <$> argument auto (metavar "AMT")
rebalanceCommand = pure Rebalance
configCommand = subparser
                (  command "show" (info configShowCommand (progDesc "show current configuration"))
                <> command "set" (info configSetCommand (progDesc "set new configuration"))
                )
configShowCommand = pure (Configure Show)
configSetCommand = pure (Configure Set)

configPathParser :: Parser FilePath
configPathParser = strOption
  ( long "config" <> short 'c' <> help "File path for config file" <> value "./.config.json")

processOptions :: Options -> IO ()
processOptions Options{optArgument=Buy amt, optConfigPath} = do {mconf <- readConfig optConfigPath
                                                                ; case mconf of
                                                                    Just Config{targetPortfolio, margin} -> print $ amt *^ targetPortfolio
                                                             }

processOptions Options{optArgument=Sell amt, optConfigPath} = do { mconf <- readConfig optConfigPath
                                                            ; case mconf of
                                                                Just Config{targetPortfolio, margin} -> print $ neg $ amt *^ targetPortfolio
                                                            }
--processOptions Options{optArgument=Rebalance , optConfigPath} = _
processOptions Options{optArgument=Configure Show, optConfigPath} = do {mconf <- readConfig optConfigPath
                                                                   ; case mconf of
                                                                       Just conf -> B.putStrLn . encodePretty $  conf
                                                                       Nothing -> print "Config not found"
                                                                   }
processOptions Options{optArgument=Configure Set, optConfigPath} = do {conf <- promptConfig; writeConfig optConfigPath conf}

main :: IO ()
main = do { options <- execParser (info opts ( fullDesc
                                      <> progDesc "Compute investment purchases for target portfolio"
                                      <> header "hello - a test for optparse-applicative" ))
          ; processOptions options
          }
