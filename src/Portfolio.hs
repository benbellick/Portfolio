{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Portfolio
    ( Portfolio(..),
      Quantity(..),
      (*^),
      (+^),
      (-^),
      valuation,
      neg
    ) where
import GHC.Generics
import Data.Aeson
import qualified Data.Map as Map
data Quantity = Dollar | Percent deriving (Eq, Generic, ToJSON, FromJSON)
  
--Record so that we can use auto deriving
data Portfolio = Portfolio
  { portfolio :: Map.Map String Double
  , unit :: Quantity
  } deriving (Generic, ToJSON, FromJSON)

instance Show Portfolio where
  show (Portfolio m q) = Map.foldrWithKey stringify "" m
    where stringify k v "" = k ++ ": " ++ showQuant q v
          stringify k v str = k ++ ": " ++ showQuant q v ++ "\n" ++ str
          showQuant Dollar v = "$" ++ show v
          showQuant Percent v = "%" ++ show v

(*^) :: Double -> Portfolio -> Portfolio
(*^) n (Portfolio m q) = Portfolio  ((*n) <$> m) q

valuation :: Portfolio -> Double
valuation (Portfolio m _) = foldr (+) 0 m

(+^), (-^) :: Portfolio -> Portfolio -> Portfolio
(+^) (Portfolio m1 q1) (Portfolio m2 q2)
  | q1 == q2 = Portfolio  (Map.unionWith (+) m1 m2) q1
  | otherwise = error "Cannot sum $ portfolio with % portfolio"
(-^) p1 p2 = (+^) p1 $ neg p2

neg :: Portfolio -> Portfolio
neg (Portfolio m q) = Portfolio (fmap (* (-1)) m) q
