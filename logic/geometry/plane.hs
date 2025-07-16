module Logic.Geometry.Plane (
  Plane(..),
  planeFromPoints,
  isPointOnPlane
) where

import Logic.Geometry.Point
import Logic.Vector (Vector, vectorSub, crossProduct, dotProduct)

-- | Plane defined by a base point and a normal vector
data Plane a = Plane (Point a) (Vector a)
  deriving (Show, Eq)

-- | Construct a plane from three points (must be non-collinear)
planeFromPoints :: (Num a, Eq a) => Point a -> Point a -> Point a -> Maybe (Plane a)
planeFromPoints p1 p2 p3
  | pointDimension p1 /= pointDimension p2 || pointDimension p1 /= pointDimension p3 = Nothing
  | otherwise = do
      let v1 = vectorFromPoint p2
          v2 = vectorFromPoint p3
          v0 = vectorFromPoint p1
      d1 <- vectorSub v1 v0
      d2 <- vectorSub v2 v0
      normal <- crossProduct d1 d2
      if all (== 0) normal
         then Nothing  -- points are collinear
         else return (Plane p1 normal)

-- | Check if a point lies on the plane (dot product of normal with (p - base) == 0)
isPointOnPlane :: (Num a, Eq a) => Plane a -> Point a -> Bool
isPointOnPlane (Plane base normal) pt =
  case vectorSub (vectorFromPoint pt) (vectorFromPoint base) of
    Just v  -> dotProduct normal v == Just 0
    Nothing -> False