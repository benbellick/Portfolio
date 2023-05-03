{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Portfolio
    ( Portfolio(..),--TODO: hide this implementation
      Quantity(..),
      (*^),
      (+^),
      (-^),
      valuation,
      rebalance,
      neg,
      valid
    ) where
import           Data.Aeson
import qualified Data.Map     as Map
import           GHC.Generics
data Quantity = Dollar | Percent deriving (Show, Eq, Generic, ToJSON, FromJSON)
-- Below type would be nice down the line, but then we have to implement our own To/FromJSON instances..
--data Item = Ticker String | Cash | Debt
--Record so that we can use auto deriving
data Portfolio = Portfolio
  { portfolio :: Map.Map String Double -- This may contain special keyword DEBT which should be negative
  , unit      :: Quantity
  } deriving (Generic, ToJSON, FromJSON)

instance Show Portfolio where
  show (Portfolio m q) = Map.foldrWithKey stringify "" m
    where stringify k v ""  = k ++ ": " ++ showQuant q v
          stringify k v str = k ++ ": " ++ showQuant q v ++ "\n" ++ str
          showQuant Dollar v
            | v >= 0 = "$" ++ show v
            | otherwise = "($" ++ show (-v) ++ ")"
          showQuant Percent v
            | v >= 0 = "%" ++ show v
            | otherwise = "(%" ++ show (-v) ++ ")"

(*^) :: Double -> Portfolio -> Portfolio
(*^) n (Portfolio m Percent) = Portfolio  ((*(n/100)) <$> m) Dollar
(*^) n (Portfolio m Dollar)  = Portfolio  ((*n) <$> m) Dollar

valuation :: Portfolio -> Double
valuation (Portfolio m _) = sum m

valid, validDebt :: Portfolio -> Bool
valid p@(Portfolio _ Percent) = valuation p == 100 && validDebt p
valid p@(Portfolio _ Dollar)  = validDebt p

validDebt (Portfolio m _) = maybe True (<=0) (Map.lookup "DEBT" m)

(+^), (-^) :: Portfolio -> Portfolio -> Portfolio
(+^) (Portfolio m1 q1) (Portfolio m2 q2)
  | q1 == q2 = Portfolio  (Map.unionWith (+) m1 m2) q1
  | otherwise = error "Cannot sum/subtract $ portfolio with % portfolio"
(-^) p1 p2 = (+^) p1 $ neg p2

neg :: Portfolio -> Portfolio
neg (Portfolio m q) = Portfolio ((* (-1)) <$> m) q

rebalance :: Portfolio -> Portfolio -> Portfolio
rebalance current@(Portfolio _ Dollar) target@(Portfolio _ Percent) = (valuation current *^ target) -^ current
rebalance _ _ = error "it only makes sense to rebalance a portfolio in $ to target a specific set of %s. Report to developer"
