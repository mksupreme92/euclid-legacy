-- | Logic.Geometry.Point
-- Euclid Geometry Kernel — Point Definition
-- Design Rule: Any function that operates on geometric data types (points, lines, etc.)
-- must receive the Space as an argument, or embed it within the structure —
-- to ensure dimensionality, metric, and context are respected.

module Logic.Geometry.Point
  ( Point
  , pointFromList
  , vectorFromPoint
  , pointDimension
  , isValidPoint
  , assertPointInSpace
  , translatePoint
  , distanceBetween
  , pointAdd
  , pointSubtract
  , definePoint
  ) where

import Logic.Vector
import Logic.Space

-- | A point is just a vector of coordinates
-- We define a type alias for semantic clarity

type Point a = Vector a

-- | Constructor
pointFromList :: [a] -> Point a
pointFromList = vectorFromList

-- | Convert a Point to a Vector
vectorFromPoint :: Point a -> Vector a
vectorFromPoint = id


-- | Get the dimension of a point
pointDimension :: Point a -> Int
pointDimension = length

-- | Check if a point is valid in a given space
isValidPoint :: Space a -> Point a -> Bool
isValidPoint space p = pointDimension p == dim space

-- | Assert that a point is valid in space, return Nothing if not
assertPointInSpace :: Space a -> Point a -> Maybe (Point a)
assertPointInSpace space p = if isValidPoint space p then Just p else Nothing

-- | Translate a point by a displacement vector (space-aware)
translatePoint :: Num a => Space a -> Point a -> Vector a -> Maybe (Point a)
translatePoint space p v =
  if pointDimension p == length v && isValidPoint space p
    then vectorAdd [p, v]
    else Nothing

-- | Distance between two points in a given space (curved or flat)
distanceBetween :: Floating a => Space a -> Point a -> Point a -> Maybe a
distanceBetween space p1 p2 =
  distanceIn space p1 p1 p2

-- Adds a vector to a point
pointAdd :: Num a => Point a -> [a] -> Maybe (Point a)
pointAdd pt vec
  | length pt == length vec = Just (zipWith (+) pt vec)
  | otherwise = Nothing

-- | Subtract two points to get a displacement vector
pointSubtract :: Num a => Point a -> Point a -> Maybe (Vector a)
pointSubtract p1 p0
  | length p1 == length p0 = Just (zipWith (-) p1 p0)
  | otherwise               = Nothing










-- | Validated constructor for a point in a given space
definePoint :: Space a -> [a] -> Maybe (Point a)
definePoint space coordinates =
  let pt = pointFromList coordinates
  in if isValidPoint space pt
        then Just pt
        else Nothing

