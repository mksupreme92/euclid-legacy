module Main where

import System.IO
import Logic.Geometry.Volume
import Logic.Geometry.Point (Point, pointFromList)
import qualified Data.List as List
import Logic.Space
import Logic.Geometry.Volume (Volume(..))

unitSphere :: Floating a => Volume a
unitSphere = ParametricVolume
  { volumeFunc = \(r, theta, phi) ->
      let x = r * sin theta * cos phi
          y = r * sin theta * sin phi
          z = r * cos theta
      in [x, y, z],
    volumeDomain = ((0, 0, 0), (1, pi, 2 * pi))
  }

-- Sample function to write a .mesh file from a volume mesh
writeMeshFile :: FilePath -> VolumeMesh Double -> IO ()
writeMeshFile path (VolumeMesh vertices tets) = withFile path WriteMode $ \h -> do
  hPutStrLn h "MeshVersionFormatted 1"
  hPutStrLn h "Dimension 3"

  -- Write vertices
  hPutStrLn h $ "Vertices"
  hPutStrLn h $ show (length vertices)
  mapM_ (\v -> hPutStrLn h $ unwords (map show v) ++ " 0") vertices

  -- Write tetrahedra
  hPutStrLn h $ "Tetrahedra"
  hPutStrLn h $ show (length tets)
  mapM_ (\tet -> hPutStrLn h $ unwords (map (show . (+1)) tet) ++ " 0") tets

  hPutStrLn h "End"

-- Define a simple cube volume for testing
unitCube :: Volume Double
unitCube = ParametricVolume
  { volumeFunc = \(u,v,w) -> [u,v,w]
  , volumeDomain = ((0, 0, 0), (1, 1, 1))
  }

unitPyramid :: Volume Double
unitPyramid = ParametricVolume
  { volumeFunc = \(u,v,w) ->
      let x = (1 - w) * (u - 0.5)
          y = (1 - w) * (v - 0.5)
          z = w
      in [x, y, z],
    volumeDomain = ((0, 0, 0), (1, 1, 1))
  }

main :: IO ()
main = do
  putStrLn "Exporting volume meshes to output/*.mesh..."
  let mesh = structuredTetrahedralMesh unitCube (2, 2, 2)
  writeMeshFile "output/cube.mesh" mesh
  putStrLn "✅ Exported cube.mesh"
  let pyramidMesh = structuredTetrahedralMesh unitPyramid (2, 2, 2)
  exportVolumeMeshToMesh "output/pyramid.mesh" pyramidMesh
  putStrLn "✅ Exported pyramid.mesh"

  let sphere = structuredTetrahedralMesh unitSphere (20, 20, 20)

  exportVolumeMeshToMesh "output/sphere.mesh" sphere
  putStrLn "✅ Exported sphere.mesh"
