module Logic.Geometry.Face (
  Face(..),
  BoundingBox,
  faceFromPoints,
  isPointOnFace,
  --facePlane,
  --faceVertices,
  boundingBox,
  isSimplePolygon,
  enforceCCWWinding,
  checkConvexity
) where

import Logic.Geometry.Point
import Logic.Geometry.Plane
import Logic.Vector


-- | A face is a set of coplanar, ordered vertices
-- Assumes the polygon is simple (non-intersecting, not self-overlapping)
data Face a = Face
  { facePlane    :: Plane a      -- Underlying plane
  , faceVertices :: [Point a]    -- Ordered boundary points
  } deriving (Show, Eq)

-- Axis-aligned bounding box as min and max corner points
type BoundingBox a = (Point a, Point a)

-- | Construct a face from a list of points (must be >= 3, coplanar, same dim)
-- Enforces counter-clockwise winding and disallows self-intersecting polygons
faceFromPoints :: (Floating a, Ord a) => [Point a] -> Maybe (Face a)
faceFromPoints pts
  | length pts < 3 = Nothing
  | not (allSameDim pts) = Nothing
  | otherwise = case planeFromPoints (pts !! 0) (pts !! 1) (pts !! 2) of
      Just pl ->
        if all (isPointOnPlane pl) pts && isSimplePolygon pts then
          let orderedPts = enforceCCWWinding pl pts
              candidateFace = Face pl orderedPts
          in if checkConvexity candidateFace
             then Just candidateFace
             else Nothing
        else Nothing
      Nothing -> Nothing
  where
    allSameDim (x:xs) = all ((== pointDimension x) . pointDimension) xs
    allSameDim [] = True

-- | Enforce counter-clockwise winding order
enforceCCWWinding :: (Floating a, Ord a) => Plane a -> [Point a] -> [Point a]
enforceCCWWinding (Plane origin normal) pts =
  let Just u = normalize =<< anyPerpendicular normal
      Just v = normalize =<< crossProduct normal u

      project2D pt =
        let Just vec = vectorSub (vectorFromPoint pt) (vectorFromPoint origin)
            Just x = dotProduct vec u
            Just y = dotProduct vec v
        in (x, y)

      projPts = map project2D pts
      area = 0.5 * sum (zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1)
                                 projPts (tail projPts ++ [head projPts]))
  in if area < 0 then reverse pts else pts

-- | Check if a polygon is simple (non-self-intersecting)
isSimplePolygon :: (Ord a, Floating a) => [Point a] -> Bool
isSimplePolygon pts
  | length pts < 3 = False
  | otherwise = case planeFromPoints (pts !! 0) (pts !! 1) (pts !! 2) of
      Just (Plane origin normal) ->
        let Just u = normalize =<< anyPerpendicular normal
            Just v = normalize =<< crossProduct normal u

            project2D pt =
              let Just vec = vectorSub (vectorFromPoint pt) (vectorFromPoint origin)
                  Just x = dotProduct vec u
                  Just y = dotProduct vec v
              in (x, y)

            projPts = map project2D pts
            n = length projPts
            segments = zip projPts (tail projPts ++ [head projPts])
            nonAdjacent (i, j) = abs (i - j) > 1 && abs (i - j) < n - 1
            pairs = [ ((i, segments !! i), (j, segments !! j))
                    | i <- [0..n-1], j <- [i+1..n-1], nonAdjacent (i,j) ]
        in all (\((_, (a1,a2)), (_, (b1,b2))) -> not (segmentsIntersect2D a1 a2 b1 b2)) pairs
      Nothing -> False

-- | Check if two line segments intersect
--segmentsIntersect :: (Ord a, Floating a) => Point a -> Point a -> Point a -> Point a -> Bool
--segmentsIntersect p1 p2 p3 p4 =
--  let orientation a b c = signum ((b !! 0 - a !! 0) * (c !! 1 - a !! 1) - (b !! 1 - a !! 1) * (c !! 0 - a !! 0))
--      o1 = orientation p1 p2 p3
--      o2 = orientation p1 p2 p4
--      o3 = orientation p3 p4 p1
--      o4 = orientation p3 p4 p2
--  in o1 /= o2 && o3 /= o4

segmentsIntersect2D :: (Ord a, Floating a) => (a,a) -> (a,a) -> (a,a) -> (a,a) -> Bool
segmentsIntersect2D p1 p2 p3 p4 =
  let orientation a b c = signum ((snd b - snd a) * (fst c - fst a) - (fst b - fst a) * (snd c - snd a))
      o1 = orientation p1 p2 p3
      o2 = orientation p1 p2 p4
      o3 = orientation p3 p4 p1
      o4 = orientation p3 p4 p2
  in o1 /= o2 && o3 /= o4

-- | Check if a point lies on the face (must lie on plane and within boundary)
-- Currently only checks planar membership — polygon boundary check is TODO
isPointOnFace :: (Num a, Eq a) => Face a -> Point a -> Bool
isPointOnFace face pt = isPointOnPlane (facePlane face) pt

-- | Compute axis-aligned bounding box (min and max corner) for a Face
boundingBox :: (Ord a) => Face a -> BoundingBox a
boundingBox (Face _ pts) =
  let dims = pointDimension (head pts)
      minCoords = [minimum [p !! i | p <- pts] | i <- [0 .. dims - 1]]
      maxCoords = [maximum [p !! i | p <- pts] | i <- [0 .. dims - 1]]
  in (minCoords, maxCoords)

-- | Check if a face is convex using sign consistency of edge cross products in 2D
checkConvexity :: (Floating a, Ord a) => Face a -> Bool
checkConvexity (Face (Plane origin normal) pts)
  | length pts < 3 = False
  | otherwise =
      let Just u = normalize =<< anyPerpendicular normal
          Just v = normalize =<< crossProduct normal u

          project2D pt =
            let Just vec = vectorSub (vectorFromPoint pt) (vectorFromPoint origin)
                Just x = dotProduct vec u
                Just y = dotProduct vec v
            in (x, y)

          projected = map project2D pts
          n = length projected

          crossZ (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

          -- Compute cross product between edges at each vertex
          turns = [ let a = projected !! i
                        b = projected !! ((i + 1) `mod` n)
                        c = projected !! ((i + 2) `mod` n)
                        ab = (fst b - fst a, snd b - snd a)
                        bc = (fst c - fst b, snd c - snd b)
                    in crossZ ab bc
                  | i <- [0 .. n - 1]
                  ]

          signs = filter (/= 0) (map signum turns)
      in not (null signs) && all (> 0) signs

-- | Helper to get a perpendicular vector (not necessarily normalized)
anyPerpendicular :: (Num a, Eq a) => [a] -> Maybe [a]
anyPerpendicular [x, y, z]
  | x /= 0 || y /= 0 = Just [-y, x, 0]
  | z /= 0           = Just [0, -z, y]
  | otherwise        = Nothing
anyPerpendicular _ = Nothing