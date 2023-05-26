{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import           Config
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Options.Applicative
import           Portfolio

data Options = Options
  { optArgument   :: Argument,
    optConfigPath :: FilePath,
    optPortfolioName :: String
  }

data ConfigCommand = Show | New | Update
data Argument = Buy Double
              | Sell Double
              | Rebalance
              | Configure ConfigCommand

opts :: Parser Options
opts = Options <$> argumentParser <*> configPathParser <*> portfolioNameParser

argumentParser :: Parser Argument
argumentParser = subparser
                 (  command "buy" (info buyCommand (progDesc "buy a specific value of target portfolio"))
                 <> command "sell" (info sellCommand (progDesc "sell a specific value of target portfolio"))
                 <> command "rebalance" (info rebalanceCommand (progDesc "rebalance portfolio"))
                 <> command "config" (info configCommand (progDesc "configure"))
                 )
buyCommand, sellCommand, rebalanceCommand, configCommand, configShowCommand, configNewCommand, configUpdateCommand :: Parser Argument
buyCommand = Buy <$> argument auto (metavar "AMT")
sellCommand = Sell <$> argument auto (metavar "AMT")
rebalanceCommand = pure Rebalance
configCommand = subparser
                (  command "show" (info configShowCommand (progDesc "show current configuration"))
                <> command "new" (info configNewCommand (progDesc "set new configuration"))
                <> command "update" (info configUpdateCommand (progDesc "update current configuration"))
                )
configShowCommand = pure (Configure Show)
configNewCommand  = pure (Configure New)
configUpdateCommand  = pure (Configure Update)

configPathParser :: Parser FilePath
configPathParser = strOption
  ( long "config" <> short 'c' <> help "File path for config file" <> value "./.config.json")

portfolioNameParser :: Parser String
portfolioNameParser = strOption
  ( long "portfolio-name" <> short 'n' <> help "Name of the portfolio to use / update" <> value "DEFAULT" )--this can be better

processOptions :: Options -> MaybeT IO ()
processOptions Options{optArgument=Rebalance, optConfigPath, optPortfolioName} = do conf <- readConfig optConfigPath
                                                                                    curPort <- lift $ promptPortfolio Dollar
                                                                                    let targetPortfolioConfM = getPortfolioByName optPortfolioName conf
                                                                                    case targetPortfolioConfM of
                                                                                      Nothing -> lift $ putStrLn "No target portfolio found with that name"
                                                                                      Just targetPortfolioConf -> let sugDiff = rebalance curPort (targetPortfolio targetPortfolioConf) in lift $ print sugDiff
processOptions Options{optArgument=Configure New, optConfigPath, optPortfolioName} = lift $ promptNewConfig >>= writeConfig optConfigPath
processOptions Options{optArgument=Configure Update, optConfigPath, optPortfolioName} = lift $ promptUpdateConfig optConfigPath optPortfolioName  >>= writeConfig optConfigPath
processOptions Options{optArgument=Configure Show, optConfigPath} = readConfig optConfigPath >>= lift . B.putStrLn . encodePretty
processOptions Options{optArgument=Buy amt, optConfigPath, optPortfolioName} = do
  conf <- readConfig optConfigPath
  let targetPortfolioConfM = getPortfolioByName optPortfolioName conf
  case targetPortfolioConfM of
    Nothing -> lift $ putStrLn "No target portfolio found with that name"
    Just targetPortfolioConf -> lift . print $ (amt * (1.0 + margin targetPortfolioConf)) *^ (targetPortfolio targetPortfolioConf)
processOptions Options{optArgument=Sell amt, optConfigPath, optPortfolioName} = do
  conf <- readConfig optConfigPath
  let targetPortfolioConfM = getPortfolioByName optPortfolioName conf
  case targetPortfolioConfM of
    Nothing -> lift $ putStrLn "No target portfolio found with that name"
    Just targetPortfolioConf -> lift . print $ neg $ (amt * (1.0 + margin targetPortfolioConf)) *^ (targetPortfolio targetPortfolioConf)

main :: IO ()
main = do { options <- execParser (info opts ( fullDesc
                                      <> progDesc "Compute investment purchases for target portfolio"
                                      <> header "hello - a test for optparse-applicative" ))
          ; mResult <- runMaybeT $ processOptions options
          ; putStrLn $ case mResult of
              Nothing -> "Error" --surely this can be better
              Just _  -> ""
          }
