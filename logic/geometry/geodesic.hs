-- logic/geometry/geodesic.hs
module Geometry.Geodesic where

import Logic.Space
import Logic.Vector
import Logic.Metric
import Geometry.Point

-- | A geodesic path is defined by:
--   - A starting point
--   - A starting velocity vector
--   - The space (and its metric) it moves in
--   - A function to compute its path parameterized by 't'
data Geodesic = Geodesic
  { initialPoint   :: Point
  , initialTangent :: Vector
  , space          :: Space
  }

-- | Evaluate the geodesic at parameter 't' using numerical integration
--   (eventually using Runge-Kutta or similar)
evaluateGeodesic :: Geodesic -> Double -> Point
evaluateGeodesic = error "Not yet implemented"

-- | Specialized constructor for flat space (Euclidean)
--   In Euclidean space, geodesics are straight lines
euclideanGeodesic :: Space -> Point -> Vector -> Geodesic
euclideanGeodesic sp p0 v0 = Geodesic p0 v0 sp