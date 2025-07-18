-- | Logic.Geometry.Line
-- Euclid Geometry Kernel — Line Definition
-- Design Rule: All geometric operations are space-aware and dimension-safe.

module Logic.Geometry.Line
  ( Line(..)
  , defineLine
  , defineLineFromPoints
  ) where

import Logic.Space
import Logic.Vector
import Logic.Geometry.Point

-- | A line is defined by a point and a direction vector
data Line a = Line
  { linePoint     :: Point a
  , lineDirection :: Vector a
  } deriving (Show, Eq)






-- | Safe constructor: Define a line from a point and a direction vector
defineLine :: Space a -> Point a -> Vector a -> Maybe (Line a)
defineLine space p dir =
  if isValidPoint space p && length dir == dim space
     then Just (Line p dir)
     else Nothing

-- | Safe constructor: Define a line from two distinct points
defineLineFromPoints :: (Eq a, Num a) => Space a -> Point a -> Point a -> Maybe (Line a)
defineLineFromPoints space p1 p2 =
  if isValidPoint space p1 && isValidPoint space p2 && p1 /= p2
     then do
       dir <- vectorSub p2 p1
       defineLine space p1 dir
     else Nothing