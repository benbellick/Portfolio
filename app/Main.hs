module Main (main) where

import Lib
import Options.Applicative

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
    options <- execParser (info opts ( fullDesc
                                      <> progDesc "Compute investment purchases for target portfolio"
                                      <> header "hello - a test for optparse-applicative" ))
    putStrLn "not implemented"
