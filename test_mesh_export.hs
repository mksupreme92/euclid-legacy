module Main where

import Logic.Geometry.Surface
import Logic.Metric
import Logic.Space (Space(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Example saddle surface for testing
saddleSurface :: Surface Double
saddleSurface = ParametricSurface
  { surfaceFunc = \(u, v) -> [u, v, u * u - v * v]
  , surfaceDomain = ((-1, -1), (1, 1))
  , surfaceSpace = Space 3 (ConstantMetric [[1,0,0],[0,1,0],[0,0,1]])
  }

main :: IO ()
main = do
  let mesh = generateSurfaceMesh saddleSurface 20 20
  let objText = exportSurfaceMeshOBJ mesh
  let outDir = "output"
  let outPath = outDir </> "saddle.obj"

  createDirectoryIfMissing True outDir
  writeFile outPath objText

  putStrLn $ "✅ Exported mesh to: " ++ outPath
