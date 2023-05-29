{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Config(
  Config(..),
  PortfolioConfig(..),
  PortfolioName,
  readConfig,
  writeConfig,
  promptPortfolio,
  promptNewConfig,
  promptUpdateConfig,
  getPortfolioByName
  ) where
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy      as B
import qualified Data.Map                  as Map
import           GHC.Generics
import           Portfolio

data PortfolioConfig = PortfolioConfig
                       { targetPortfolio :: Portfolio
                       , margin :: Double
                       } deriving (Show, Generic, ToJSON, FromJSON)

type PortfolioName = String
data Config = Config
  { portfolios :: Map.Map PortfolioName PortfolioConfig
  , defaultPortfolio :: PortfolioName
  } deriving (Show, Generic, ToJSON, FromJSON)

readConfig :: FilePath -> MaybeT IO Config
--TODO : when transformers upgrades to 0.6, replace (MaybeT . pure) with hoistMabe when upgrading transformers to >=0.6
readConfig path = lift (B.readFile path) >>= MaybeT . pure . decode

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = let str = encode config in B.writeFile path str

promptNewConfig :: IO Config
promptNewConfig = do putStrLn "Lets make a fresh config!"
                     portName <- promptPortfolioName
                     portfolio <- promptPortfolioConfig Percent
                     return $ Config{ portfolios=Map.fromList [(portName, portfolio)],
                                      defaultPortfolio=portName}

--TODO: Update imple to include update of default portfolio name
promptUpdateConfig :: FilePath -> String -> IO Config
promptUpdateConfig path portfolioName = do putStrLn "Lets update the existing config!"
                                           putStrLn "Enter the same name as the desired portfolio update"
                                           confM <- runMaybeT $ readConfig path
                                           case confM of
                                             Nothing -> putStrLn "Config not found, preparing fresh one" >> promptNewConfig
                                             Just conf -> do { portfolioConfig <- promptPortfolioConfig Percent
                                                            ; return $ updateConfig portfolioName portfolioConfig conf }
promptPortfolioName :: IO PortfolioName
promptPortfolioName = do putStrLn "Enter the name of the portfolio:"
                         putStrLn "(If no name is entered, \"DEFAULT\" is used)"
                         name <- getLine
                         case name of
                           "" -> return "DEFAULT"
                           _ -> return name
                         

updateConfig :: String -> PortfolioConfig -> Config -> Config
updateConfig portfolioName pc c@Config{portfolios} = c{portfolios=Map.insert portfolioName pc portfolios}

                        

promptPortfolioConfig :: Quantity -> IO PortfolioConfig
promptPortfolioConfig q = do targetPortfolio <- promptPortfolio q
                             margin <- promptMargin
                             return $ PortfolioConfig{targetPortfolio, margin}


promptPortfolio :: Quantity -> IO Portfolio
promptPortfolio q = do { putStrLn ("Enter portfolio (in " ++ show q ++ "):")
                           ; tickerPercentPair <- promptTickerPercentPair q
                           ; return $ case q of
                               Percent -> Portfolio (Map.fromList tickerPercentPair) q
                               Dollar -> Portfolio (Map.fromList tickerPercentPair) q
                           }

promptTickerPercentPair :: Quantity -> IO [(String, Double)]
promptTickerPercentPair q = do { putStrLn "Ticker:"
                             ; ticker <- getLine
                             ; case ticker of
                                 "" -> return []
                                 _ -> do { putStrLn $ case q of
                                            Percent -> "%:"
                                            Dollar  -> "$:"
                                        ; percent <- readLn :: IO Double
                                        ; p <- promptTickerPercentPair q
                                        ; return $ (ticker,percent):p
                                        }
                             }

promptMargin :: IO Double
promptMargin = do { putStrLn "Would you like to include margin? Enter nothing for no"
                  ; marginStr <- getLine
                  ; case marginStr of
                      "" -> return 0
                      _  -> return $ read marginStr
                  }

getPortfolioByName :: String -> Config -> Maybe PortfolioConfig
getPortfolioByName name conf = Map.lookup name (portfolios conf)


