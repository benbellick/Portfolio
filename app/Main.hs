{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Portfolio
import qualified Data.Map as Data.Map
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import Config

data Options = Options
  { optArgument :: Argument,
    optConfigPath :: FilePath
  }

data Argument = Buy Double
              | Sell Double
              | Rebalance
              | ShowConfig
              | SetConfig
              deriving (Show, Read)

opts :: Parser Options
opts = Options <$> commandParser <*> configPathParser

commandParser :: Parser Argument
commandParser = argument auto (metavar "COMMAND")

configPathParser :: Parser FilePath
configPathParser = strOption
  ( long "config" <> short 'c' <> help "File path for config file" <> value "./.config.json")

processOptions :: Options -> IO ()
processOptions Options{optArgument=Buy amt, optConfigPath} = do {mconf <- readConfig optConfigPath
                                                                ; case mconf of
                                                                    Just Config{targetPortfolio, margin} -> putStrLn . show $ amt *^ targetPortfolio
                                                             }
processOptions Options{optArgument=Sell amt, optConfigPath} = do { mconf <- readConfig optConfigPath
                                                            ; case mconf of
                                                                Just Config{targetPortfolio, margin} -> putStrLn . show $ neg $ amt *^ targetPortfolio
                                                            }
--processOptions Options{optArgument=Rebalance , optConfigPath} = _
processOptions Options{optArgument=ShowConfig, optConfigPath} = do {mconf <- readConfig optConfigPath
                                                                   ; case mconf of
                                                                       Just conf -> B.putStrLn . encodePretty $  conf
                                                                       Nothing -> print "Config not found"
                                                                   }
processOptions Options{optArgument=SetConfig, optConfigPath} = do {conf <- promptConfig; writeConfig optConfigPath conf}

main :: IO ()
main = do { options <- execParser (info opts ( fullDesc
                                      <> progDesc "Compute investment purchases for target portfolio"
                                      <> header "hello - a test for optparse-applicative" ))
          ; processOptions options
          }
