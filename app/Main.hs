module Main (main) where

import qualified Portfolio as P
import qualified Data.Map as Data.Map
import qualified Data.Aeson as JSON
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import Config


targetPortfolio :: P.Portfolio
targetPortfolio = Data.Map.fromList [("VTI", 0.40),("AVUV", 0.20),("VXUS", 0.27),("AVDV", 0.13)]

buyAmt :: Parser Double
buyAmt = option auto
         ( long "purchase quantity"
        <> short 'p'
        <> help "amount desired of purchase" )

margin :: Parser Double
margin = option auto
         ( long "margin amount"
        <> short 'm'
        <> help "margin position size"
        <> showDefault 
        <> value 0)

data Options = Options
    { optBuyAmt :: Double
    , optMargin :: Double }

opts :: Parser Options
opts = Options <$> buyAmt <*> margin

main :: IO ()
main = do
    c <- promptConfig
    options <- execParser (info opts ( fullDesc
                                      <> progDesc "Compute investment purchases for target portfolio"
                                      <> header "hello - a test for optparse-applicative" ))
    B.putStrLn (JSON.encode (purchaseSuggestion options targetPortfolio))

purchaseSuggestion :: Options -> P.Portfolio -> P.Portfolio
purchaseSuggestion o p = (optBuyAmt o * (1 + optMargin o)) P.*^ p
