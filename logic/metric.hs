module Logic.Metric
  ( MetricType(..)
  , innerProductAt
  , distanceAt
  ) where

import Logic.Vector
import Logic.Matrix

-- | A metric can either be constant (flat space) or vary with position (curved space)
data MetricType a
  = ConstantMetric (Matrix a)
  | FieldMetric (Vector a -> Matrix a)

-- | Computes inner product ⟨v, w⟩ at point p using the metric
innerProductAt :: Num a => MetricType a -> Vector a -> Vector a -> Vector a -> Maybe a
innerProductAt (ConstantMetric g) _ v w = do
  gv <- matrixVectorProduct g v
  dotProduct gv w

innerProductAt (FieldMetric f) p v w = do
  let g = f p
  gv <- matrixVectorProduct g v
  dotProduct gv w

-- | Computes distance using inner product at point p (norm of v - w)
distanceAt :: Floating a => MetricType a -> Vector a -> Vector a -> Vector a -> Maybe a
distanceAt metric p v w = do
  delta <- vectorSub v w
  ip <- innerProductAt metric p delta delta
  return (sqrt ip)