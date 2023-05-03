{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Config(
  Config(..),
  readConfig,
  writeConfig,
  promptPortfolio,
  promptConfig
  ) where
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy      as B
import qualified Data.Map                  as Map
import           GHC.Generics
import           Portfolio

data Config = Config
  { targetPortfolio :: Portfolio
  , margin          :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

readConfig :: FilePath -> MaybeT IO Config
--TODO : when transformers upgrades to 0.6, replace (MaybeT . pure) with hoistMabe when upgrading transformers to >=0.6
readConfig path = lift (B.readFile path) >>= MaybeT . pure . decode

writeConfig :: FilePath -> Config -> IO ()
writeConfig path config = let str = encode config in B.writeFile path str

promptConfig :: IO Config
promptConfig = do { putStrLn "Lets make a config"
                  ; targetPortfolio <- promptPortfolio Percent
                  ; margin <- promptMargin
                  ; return Config{targetPortfolio, margin}
                  }

promptPortfolio :: Quantity -> IO Portfolio
promptPortfolio q = do { putStrLn ("Enter portfolio (in " ++ show q ++ "):")
                           ; tickerPercentPair <- promptTickerPercentPair q
                           ; return $ case q of
                               Percent -> Portfolio (Map.fromList (convertNumToPercent tickerPercentPair)) q
                               Dollar -> Portfolio (Map.fromList tickerPercentPair) q
                           }
  where convertNumToPercent = map (fmap (/100))

promptTickerPercentPair :: Quantity -> IO [(String, Double)]
promptTickerPercentPair q = do { putStrLn "Ticker:"
                             ; ticker <- getLine
                             ; case ticker of
                                 "" -> return []
                                 _ -> do { putStrLn $ case q of
                                            Percent -> "%:"
                                            Dollar -> "$"
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
