-- | Logic.Geometry.Curve
-- Euclid Geometry Kernel — Parametric Curve Definitions

module Logic.Geometry.Curve where

import Logic.Space
import Logic.Vector
import Logic.Geometry.Point

-- | A parametric curve defined over a parameter interval [t0, t1]
--   curveFunc: a function from parameter t to a point in space
--   space: the space this curve resides in
data Curve a = Curve
  { curveFunc  :: Double -> Point a
  , domain     :: (Double, Double)
  , curveSpace :: Space a
  }

-- | Evaluate the curve at a given parameter t
evaluateCurve :: Curve a -> Double -> Point a
evaluateCurve c t = curveFunc c t

-- | Construct a linear parametric curve from point p0 to p1 in space
linearCurve :: (Floating a, Eq a) => Space a -> Point a -> Point a -> Maybe (Curve a)
linearCurve sp p0 p1 =
  let
    -- Ensure both points are in the same space
    _ = assertPointInSpace sp p0
    _ = assertPointInSpace sp p1

    -- Compute vector from p0 to p1
    v = vectorFromPoint sp p1 >>= \vp1 ->
        vectorFromPoint sp p0 >>= \vp0 ->
        Just (vectorSub vp1 vp0)

    -- Function from parameter t ∈ [0,1] to point on the line
    f t = do
      dv <- v
      let scaled = scalarMul t dv
      translatePoint sp p0 scaled
  in
    Just $ Curve f (0, 1) sp