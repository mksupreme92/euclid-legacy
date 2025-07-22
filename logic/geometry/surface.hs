module Logic.Geometry.Surface where

import Logic.Space (Space(..))
import qualified Logic.Space as Space
import Data.List (nub)

-- Parametric surface: ℝ² → ℝⁿ
data Surface a = ParametricSurface
  { surfaceFunc   :: (a, a) -> [a]            -- S(u, v): ℝ² → ℝⁿ
  , surfaceDomain :: ((a, a), (a, a))         -- ((umin, vmin), (umax, vmax))
  , surfaceSpace  :: Space a                  -- Ambient space info
  }

-- Evaluate the surface at (u, v)
evaluateSurface :: Surface a -> a -> a -> [a]
evaluateSurface surf u v = surfaceFunc surf (u, v)


-- | Discretized mesh for a parametric surface
data SurfaceMesh a = SurfaceMesh
  { meshVertices :: [[a]]         -- List of vertex coordinates (flattened ℝⁿ)
  , meshFaces    :: [[Int]]       -- Connectivity: faces as indices into vertices
  , meshSpace    :: Space a       -- Ambient space
  }

-- | Generate a mesh from a parametric surface by sampling it at a grid of (u,v) values.
generateSurfaceMesh :: (RealFrac a, Enum a) => Surface a -> Int -> Int -> SurfaceMesh a
generateSurfaceMesh surface uSteps vSteps =
  let ((umin, vmin), (umax, vmax)) = surfaceDomain surface
      us = [umin, umin + stepU .. umax]
      vs = [vmin, vmin + stepV .. vmax]
      stepU = (umax - umin) / fromIntegral (uSteps - 1)
      stepV = (vmax - vmin) / fromIntegral (vSteps - 1)

      -- Generate vertex grid
      verts = [ evaluateSurface surface u v | v <- vs, u <- us ]

      -- Compute face indices for quads (split into triangles)
      width = length us
      height = length vs

      idx i j = i + j * width

      faces =
        [ [idx i j, idx (i+1) j, idx (i+1) (j+1)]
        | j <- [0..height-2], i <- [0..width-2] ] ++
        [ [idx i j, idx (i+1) (j+1), idx i (j+1)]
        | j <- [0..height-2], i <- [0..width-2] ]

  in SurfaceMesh verts faces (surfaceSpace surface)


-- Validate a surface mesh
validateSurfaceMesh :: SurfaceMesh a -> Bool
validateSurfaceMesh (SurfaceMesh vertices faces _) =
  let numVertices = length vertices
      faceValid face =
        all (\i -> i >= 0 && i < numVertices) face &&     -- all indices in range
        length (nub face) == length face &&               -- no duplicates
        length face >= 3                                  -- at least a triangle
  in all faceValid faces


-- Export SurfaceMesh to Wavefront OBJ format as a String
exportSurfaceMeshOBJ :: (Show a) => SurfaceMesh a -> Either String String
exportSurfaceMeshOBJ (SurfaceMesh vertices faces _) =
    if all (\v -> length v == 3) vertices
    then
      let vLines = map (\v -> "v " ++ unwords (map show v)) vertices
          fLines = map (\f -> "f " ++ unwords (map (show . (+1)) f)) faces
      in Right (unlines (vLines ++ fLines))
    else
      Left "OBJ export only supports 3D surfaces. Found vertex with dimension ≠ 3."