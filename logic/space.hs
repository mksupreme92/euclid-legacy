module Logic.Space
  ( Space(..)
  , normAt
  , distanceIn
  , isOrthogonal
  ) where

import Logic.Vector
import Logic.Metric

-- | A space is defined by its dimension and a metric
data Space a = Space
  { dim    :: Int
  , metric :: MetricType a
  }

-- | Computes the norm of a vector v at point p in the space
normAt :: Floating a => Space a -> Vector a -> Vector a -> Maybe a
normAt (Space _ m) p v = do
  ip <- innerProductAt m p v v
  return (sqrt ip)

-- | Computes the distance between two vectors at point p
distanceIn :: Floating a => Space a -> Vector a -> Vector a -> Vector a -> Maybe a
distanceIn (Space _ m) p v w = distanceAt m p v w

-- | Determines if two vectors are orthogonal at point p
isOrthogonal :: (Eq a, Num a) => Space a -> Vector a -> Vector a -> Vector a -> Bool
isOrthogonal (Space _ m) p v w =
  case innerProductAt m p v w of
    Just 0 -> True
    _      -> False