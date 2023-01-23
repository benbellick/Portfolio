module Portfolio
    ( Portfolio,
      multiply,
      value
    ) where

import qualified Data.Map as Map

type Portfolio = Map.Map String Double 

multiply :: Double -> Portfolio -> Portfolio
multiply n p = (*n) <$> p

value :: Portfolio -> Double
value p = foldr (+) 0 p
