{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
module Config(
  Config(..),
  readConfig,
  writeConfig,
  promptConfig
  ) where
import Portfolio
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import qualified Data.Map as Map

data Config = Config
  { targetPortfolio :: Portfolio
  , margin :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

readConfig :: FilePath -> IO (Maybe Config)
readConfig path = do { str <- B.readFile path
                     ; return $ decode str
                     }

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = let str = encode config in B.writeFile path str              

promptConfig :: IO Config
promptConfig = do { putStrLn "Lets make a config"
                  ; targetPortfolio <- promptTargetPortfolio
                  ; margin <- promptMargin
                  ; return Config{targetPortfolio, margin}
                  }

promptTargetPortfolio :: IO Portfolio
promptTargetPortfolio = do { putStrLn "Enter portfolio (in percentages):"
                           ; tickerPercentPair <- promptTickerPercentPair
                           ; return $ Portfolio (Map.fromList tickerPercentPair) Percent
                           }
  
promptTickerPercentPair :: IO [(String, Double)]
promptTickerPercentPair = do { putStrLn "Ticker:"
                             ; ticker <- getLine
                             ; case ticker of
                                 "" -> return []
                                 _ -> do { putStrLn "Percent:"
                                        ; percent <- readLn :: IO Double
                                        ; p <- promptTickerPercentPair
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
