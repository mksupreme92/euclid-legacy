-- logic/geometry/LineSegment.hs

module Logic.Geometry.LineSegment
  ( LineSegment(..)
  , defineLineSegment
  ) where

import Logic.Vector (Vector, vectorSub)
import Logic.Geometry.Point (Point, isValidPoint)
import Logic.Space (Space)

-- A line segment is defined by two endpoints
data LineSegment a = LineSegment
  { startPoint :: Point a
  , endPoint   :: Point a
  } deriving (Show, Eq)

-- Safe constructor for a line segment in a given space
-- Ensures both points are valid and in the same dimension
defineLineSegment :: (Eq a, Num a) => Space a -> Point a -> Point a -> Maybe (LineSegment a)
defineLineSegment space p1 p2 =
  if isValidPoint space p1 && isValidPoint space p2
    then Just (LineSegment p1 p2)
    else Nothing