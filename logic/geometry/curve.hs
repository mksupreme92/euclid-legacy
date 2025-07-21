-- | Logic.Geometry.Curve
-- Euclid Geometry Kernel — Parametric Curve Definitions

module Logic.Geometry.Curve where

import Logic.Space
import Logic.Vector
import Logic.Geometry.Point

-- | A parametric curve defined over a parameter interval [t0, t1]
--   curveFunc: a function from parameter t to a point in space
--   space: the space this curve resides in
data Curve a
  = Curve
      { curveFunc  :: Double -> Point a
      , domain     :: (Double, Double)
      , curveSpace :: Space a
      }
  | ParametricCurve
      { parametricFunc  :: Double -> [Double]
      , parametricDomain :: (Double, Double)
      , parametricSpace  :: Space Double
      }

-- | Evaluate the curve at a given parameter t
evaluateCurve :: Curve a -> Double -> Point a
evaluateCurve c t = curveFunc c t

-- | Construct a linear parametric curve from point p0 to p1 in space
linearCurve :: Space Double -> Point Double -> Point Double -> Maybe (Curve Double)
linearCurve sp p0 p1 = do
  -- Validate points
  _ <- assertPointInSpace sp p0
  _ <- assertPointInSpace sp p1

  -- Vector difference
  let v = zipWith (-) p1 p0  -- [Double]

  -- Curve function: p(t) = p0 + t*(p1 - p0)
  let f t =
        case translatePoint sp p0 (scalarMul t v) of
          Just pt -> pt
          Nothing -> error "linearCurve: translation failed"

  return $ Curve f (0, 1) sp