module Logic.Geometry.Volume
  ( Volume(..)
  , sampleParametricVolume
  , VolumeMesh(..)
  , structuredTetrahedralMesh
  , volumeMeshVolume
  , validateVolumeMesh
  ) where

import Logic.Numerics.Sampling (sampleRectilinearGrid)
import Data.List (nub)

-- | A parametric volume maps from R^3 to R^n embedded space
data Volume a
  = ParametricVolume {
      volumeFunc :: (a, a, a) -> [a],
      volumeDomain :: ((a, a, a), (a, a, a))
    }

-- | Sample a parametric volume over a rectilinear grid
sampleParametricVolume :: (Fractional a, Enum a) => Volume a -> (Int, Int, Int) -> [[a]]
sampleParametricVolume (ParametricVolume f ((umin,vmin,wmin),(umax,vmax,wmax))) (uSteps,vSteps,wSteps) =
  let mins   = [umin, vmin, wmin]
      maxs   = [umax, vmax, wmax]
      steps  = [uSteps, vSteps, wSteps]
      grid   = sampleRectilinearGrid mins maxs steps
  in map (\[u,v,w] -> f (u,v,w)) grid

--------------------------------------------------------------------------------
-- Structured tetrahedral meshing
--------------------------------------------------------------------------------

-- | A simple structured tetrahedral mesh: vertices plus tetra connectivity
data VolumeMesh a = VolumeMesh
  { vmVertices :: [[a]]   -- ^ coordinates of vertices
  , vmTets     :: [[Int]] -- ^ each tetra is 4 indices into vmVertices
  } deriving (Eq, Show)

-- | Build a structured tetrahedral mesh from a parametric volume and a regular (u,v,w) grid.
--   Each hexahedral cell is split into 5 tetrahedra with a consistent pattern.
structuredTetrahedralMesh
  :: (Fractional a, Enum a)
  => Volume a               -- ^ parametric volume
  -> (Int, Int, Int)        -- ^ (uSteps, vSteps, wSteps)
  -> VolumeMesh a
structuredTetrahedralMesh vol@(ParametricVolume _ ((umin,vmin,wmin),(umax,vmax,wmax))) (uSteps,vSteps,wSteps) =
  let -- sample grid vertices
      verts = sampleParametricVolume vol (uSteps, vSteps, wSteps)

      nu = uSteps
      nv = vSteps
      nw = wSteps

      -- linear index of grid point (i,j,k)
      idx i j k = i + j * nu + k * nu * nv

      -- split each cube cell into 5 non-degenerate tets using vertices labelled:
      -- v000 = (i,  j,  k)
      -- v100 = (i+1,j,  k)
      -- v010 = (i,  j+1,k)
      -- v110 = (i+1,j+1,k)
      -- v001 = (i,  j,  k+1)
      -- v101 = (i+1,j,  k+1)
      -- v011 = (i,  j+1,k+1)
      -- v111 = (i+1,j+1,k+1)
      cubeTets i j k =
        let v000 = idx i     j     k
            v100 = idx (i+1) j     k
            v010 = idx i     (j+1) k
            v110 = idx (i+1) (j+1) k
            v001 = idx i     j     (k+1)
            v101 = idx (i+1) j     (k+1)
            v011 = idx i     (j+1) (k+1)
            v111 = idx (i+1) (j+1) (k+1)
        in  [ -- split the hex into 5 non-degenerate tets
              [v000, v100, v010, v001]
            , [v100, v110, v010, v111]
            , [v100, v010, v001, v111]
            , [v010, v001, v011, v111]
            , [v100, v001, v101, v111]
            ]

      allTets =
        [ cubeTets i j k
        | i <- [0 .. nu-2]
        , j <- [0 .. nv-2]
        , k <- [0 .. nw-2]
        ]
      tetList = concat allTets
  in VolumeMesh verts tetList

-- | Determinant of a 3x3 matrix given as column vectors.
det3 :: Num a => [a] -> [a] -> [a] -> a
det3 [a1,a2,a3] [b1,b2,b3] [c1,c2,c3] =
  a1*(b2*c3 - b3*c2) - a2*(b1*c3 - b3*c1) + a3*(b1*c2 - b2*c1)
det3 _ _ _ = 0  -- misuse fallback

-- | Volume of a single tetra (absolute value, /6 of parallelepiped)
tetVolume :: (Floating a) => [[a]] -> [Int] -> a
tetVolume verts [i,j,k,l] =
  case (verts !! i, verts !! j, verts !! k, verts !! l) of
    (a@[_,_,_], b@[_,_,_], c@[_,_,_], d@[_,_,_]) ->
      let ab = zipWith (-) b a
          ac = zipWith (-) c a
          ad = zipWith (-) d a
      in abs (det3 ab ac ad) / 6
    _ -> 0
tetVolume _ _ = 0

-- | Sum of all tetra volumes
volumeMeshVolume :: (Floating a) => VolumeMesh a -> a
volumeMeshVolume (VolumeMesh vs ts) = sum (map (tetVolume vs) ts)

-- | Basic validation: indices in range, 4 distinct verts, non-zero volume
validateVolumeMesh :: (Floating a, Ord a) => VolumeMesh a -> Bool
validateVolumeMesh (VolumeMesh vs ts) =
  let n = length vs
      validTet is =
        length is == 4
        && all (\ix -> ix >= 0 && ix < n) is
        && length (nub is) == 4
        && tetVolume vs is > 1e-12  -- allow tiny numeric noise
  in all validTet ts
