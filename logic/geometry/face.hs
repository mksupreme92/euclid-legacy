module Logic.Geometry.Face (
  Face,
  BoundingBox,
  faceFromPoints,
  isPointOnFace,
  facePlane,
  faceVertices,
  boundingBox,
  checkConvexity

) where

import Logic.Geometry.Point
import Logic.Geometry.Plane
import Logic.Vector
import Logic.Geometry.Point (pointAdd)
import Data.List (sortBy, nub)
import Data.Function (on)
import Debug.Trace (trace)

-- | A face is a set of coplanar, ordered vertices
-- Assumes the polygon is simple (non-intersecting, not self-overlapping)
data Face a = Face
  { facePlane    :: Plane a      -- Underlying plane
  , faceVertices :: [Point a]    -- Ordered boundary points
  } deriving (Show, Eq)

-- Axis-aligned bounding box as min and max corner points
type BoundingBox a = (Point a, Point a)

-- | Construct a face from a list of points (must be >= 3, coplanar, same dim)
faceFromPoints :: (Floating a, Ord a, Show a) => [Point a] -> Maybe (Face a)
faceFromPoints pts
  | length pts < 3 = Nothing
  | not (allSameDim pts) = Nothing
  | otherwise = case planeFromPoints (pts !! 0) (pts !! 1) (pts !! 2) of
      Just pl ->
        if all (isPointOnPlane pl) pts
          then Just (Face pl (standardizeWinding (Face pl pts)))
          else Nothing
      Nothing -> Nothing
  where
    allSameDim (x:xs) = all ((== pointDimension x) . pointDimension) xs
    allSameDim [] = True

-- | Standardize winding order of a polygon face so that it is counter-clockwise
standardizeWinding :: (Floating a, Ord a, Show a) => Face a -> [Point a]
standardizeWinding (Face (Plane origin normal) pts) =
  let Just u = normalize =<< anyPerpendicular normal
      Just v = normalize =<< crossProduct normal u

      -- Project to 2D
      project2D pt =
        let Just vec = vectorSub (vectorFromPoint pt) (vectorFromPoint origin)
            Just x = dotProduct vec u
            Just y = dotProduct vec v
        in (x, y)

      unproject2D (x, y) =
        let Just vec = vectorAdd [scalarMul x u, scalarMul y v]
            Just pt  = pointAdd origin vec
        in pt

      projPts = map project2D pts
      areaSum = sum $ zipWith (\(x1,y1) (x2,y2) -> (x1 * y2 - x2 * y1))
                              projPts (tail projPts ++ [head projPts])

      -- Use convex hull if shape is not convex
      repairedPts = if not (checkConvexity (Face (Plane origin normal) pts))
                      then map unproject2D (convexHull projPts)
                      else if areaSum < 0
                            then reverse pts
                            else pts
  in repairedPts


-- | Compute the 2D convex hull of a set of points using Graham scan
convexHull :: (Ord a, Floating a) => [(a, a)] -> [(a, a)]
convexHull pts = nub $ init lower ++ init upper
  where
    sorted = sortBy (\(x1, y1) (x2, y2) -> compare x1 x2 <> compare y1 y2) (nub pts)

    turn (x1, y1) (x2, y2) (x3, y3) =
      (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

    buildHull = foldl step []
      where
        step (p2:p1:ps) p
          | turn p1 p2 p <= 0 = step (p1:ps) p
          | otherwise         = p2:p1:ps ++ [p]
        step ps p = ps ++ [p]

    lower = buildHull sorted
    upper = buildHull (reverse sorted)


-- | Compute signed area of a 2D polygon
signedPolygonArea :: Floating a => [(a, a)] -> a
signedPolygonArea pts =
  0.5 * sum [ x1 * y2 - x2 * y1
            | ((x1, y1), (x2, y2)) <- zip pts (tail pts ++ [head pts]) ]

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

-- | Top-level convexity check: uses fast method first, falls back to robust if needed.
checkConvexity :: (Floating a, Ord a, Show a) => Face a -> Bool
checkConvexity face =
  if checkConvexitySimple face
    then True
    else trace "🧭 Falling back to checkConvexityRobust" (checkConvexityRobust face)

-- | Check if a face is convex using 2D projection and signed area method
checkConvexitySimple :: (Floating a, Ord a) => Face a -> Bool
checkConvexitySimple (Face (Plane origin normal) pts)
  | length pts < 3 = False
  | otherwise =
      let Just u = normalize =<< anyPerpendicular normal
          Just v = normalize =<< crossProduct normal u

          project2D pt =
            let Just vec = vectorSub (vectorFromPoint pt) (vectorFromPoint origin)
                Just x = dotProduct vec u
                Just y = dotProduct vec v
            in (x, y)

          projPts = map project2D pts

          signedAreas = zipWith3 (\(x1,y1) (x2,y2) (x3,y3) ->
                                    let dx1 = x2 - x1
                                        dy1 = y2 - y1
                                        dx2 = x3 - x2
                                        dy2 = y3 - y2
                                    in dx1 * dy2 - dx2 * dy1)
                        projPts
                        (tail projPts ++ [head projPts])
                        (drop 2 (cycle projPts))

          allPositive = all (> 0) signedAreas
          allNegative = all (< 0) signedAreas

      in allPositive || allNegative

-- | Robust convexity check using angle sign consistency in 2D
checkConvexityRobust :: (Floating a, Ord a) => Face a -> Bool
checkConvexityRobust (Face (Plane origin normal) pts)
  | length pts < 3 = False
  | otherwise =
      let Just u = normalize =<< anyPerpendicular normal
          Just v = normalize =<< crossProduct normal u

          project2D pt =
            let Just vec = vectorSub (vectorFromPoint pt) (vectorFromPoint origin)
                Just x = dotProduct vec u
                Just y = dotProduct vec v
            in (x, y)

          projPts = map project2D pts

          edgeVectors = zipWith (\(x1,y1) (x2,y2) -> (x2 - x1, y2 - y1))
                                 projPts
                                 (tail projPts ++ [head projPts])

          angleSigns = zipWith (\(ux,uy) (vx,vy) -> ux*vy - uy*vx)
                                edgeVectors
                                (tail edgeVectors ++ [head edgeVectors])

          allPositive = all (> 0) angleSigns
          allNegative = all (< 0) angleSigns

      in allPositive || allNegative

-- | Helper to get a perpendicular vector (not necessarily normalized)
anyPerpendicular :: (Num a, Eq a) => [a] -> Maybe [a]
anyPerpendicular [x, y, z]
  | x /= 0 || y /= 0 = Just [-y, x, 0]
  | z /= 0           = Just [0, -z, y]
  | otherwise        = Nothing
anyPerpendicular _ = Nothing
