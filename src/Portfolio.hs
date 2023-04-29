module Portfolio
    ( Portfolio,
      (*^),
      (+^),
      (-^),
      value
    ) where

import qualified Data.Map as Map

type Portfolio = Map.Map String Double

(*^) :: Double -> Portfolio -> Portfolio
(*^) n p = (*n) <$> p

value :: Portfolio -> Double
value p = foldr (+) 0 p

(+^), (-^) :: Portfolio -> Portfolio -> Portfolio
(+^) = Map.unionWith (+)
(-^) p1 p2 = (+^) p1 $ neg p2

neg :: Portfolio -> Portfolio
neg = fmap (* (-1))

