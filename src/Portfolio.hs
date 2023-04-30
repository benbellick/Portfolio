{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Portfolio
    ( Portfolio(..),
      (*^),
      (+^),
      (-^),
      valuation,
      neg
    ) where
import GHC.Generics
import Data.Aeson
import qualified Data.Map as Map
--do we want the constructor exposed?
newtype Portfolio = Portfolio(Map.Map String Double) deriving (Generic, ToJSON, FromJSON)

instance Show Portfolio where
  show (Portfolio m) = Map.foldrWithKey stringify "" m
    where stringify k v "" = k ++ ": " ++ show v
          stringify k v str = k ++ ": " ++ show v ++ "\n" ++ str

(*^) :: Double -> Portfolio -> Portfolio
(*^) n (Portfolio m) = Portfolio $ (*n) <$> m

valuation :: Portfolio -> Double
valuation (Portfolio m) = foldr (+) 0 m

(+^), (-^) :: Portfolio -> Portfolio -> Portfolio
(+^) (Portfolio m1) (Portfolio m2) = Portfolio $ Map.unionWith (+) m1 m2
(-^) p1 p2 = (+^) p1 $ neg p2

neg :: Portfolio -> Portfolio
neg (Portfolio m) = Portfolio $ fmap (* (-1)) m
