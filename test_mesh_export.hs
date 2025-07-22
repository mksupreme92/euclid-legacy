module Main where

import Logic.Geometry.Surface
import Logic.Metric
import Logic.Vector (normalize, crossProduct)
import Logic.Space (Space(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Data.Fixed (mod')

saddle :: Surface Double
saddle = ParametricSurface
  { surfaceFunc = \(u, v) -> [u, v, u * u - v * v]
  , surfaceDomain = ((-1, -1), (1, 1))
  , surfaceSpace = Space 3 (ConstantMetric [[1,0,0],[0,1,0],[0,0,1]])
  }

sinusoidalTorus :: Surface Double
sinusoidalTorus = ParametricSurface
  { surfaceFunc = \(u, v) ->
      let r1 = 0.5
          r2 = 1.5
          x = (r2 + r1 * cos v) * cos u
          y = (r2 + r1 * cos v) * sin u
          z = r1 * sin v + 0.3 * sin (3 * u)
      in [x, y, z]
  , surfaceDomain = ((0, 0), (2 * pi, 2 * pi))
  , surfaceSpace = Space 3 (ConstantMetric [[1,0,0],[0,1,0],[0,0,1]])
  }

trefoilKnot :: Surface Double
trefoilKnot = ParametricSurface
  { surfaceFunc = \(u, v) ->
      let
        -- Core curve
        cx = (2 + cos (3 * u)) * cos (2 * u)
        cy = (2 + cos (3 * u)) * sin (2 * u)
        cz = sin (3 * u)

        tx = -2 * (2 + cos (3 * u)) * sin (2 * u) - 3 * sin (3 * u) * cos (2 * u)
        ty =  2 * (2 + cos (3 * u)) * cos (2 * u) - 3 * sin (3 * u) * sin (2 * u)
        tz =  3 * cos (3 * u)
        tVec = [tx, ty, tz]

        r = 0.2
      in
        case normalize tVec of
          Nothing -> [cx, cy, cz] -- fallback if normalize fails
          Just t ->
            case crossProduct t [0, 0, 1] of
              Nothing -> [cx, cy, cz]
              Just nRaw ->
                case normalize nRaw of
                  Nothing -> [cx, cy, cz]
                  Just n ->
                    case crossProduct t n of
                      Nothing -> [cx, cy, cz]
                      Just b ->
                        let angle = v
                            offset = zipWith3 (\ni bi ang -> r * cos ang * ni + r * sin ang * bi)
                                               n b [angle, angle, angle]
                        in zipWith (+) [cx, cy, cz] offset
  , surfaceDomain = ((0, 0), (2 * pi, 2 * pi))
  , surfaceSpace = Space 3 (ConstantMetric [[1,0,0],[0,1,0],[0,0,1]])
  }

-- Parametric heart-shaped surface using the classic heart curve and z-axis thickness
heartSurface :: Surface Double
heartSurface = ParametricSurface
  { surfaceFunc = \(u, v) ->
      let x = 16 * (sin u) ** 3
          y = 13 * cos u - 5 * cos (2 * u) - 2 * cos (3 * u) - cos (4 * u)
          z = 2 * cos v
      in [x, y, z]
  , surfaceDomain = ((0, 0), (2 * pi, 2 * pi))
  , surfaceSpace = Space 3 (ConstantMetric [[1,0,0],[0,1,0],[0,0,1]])
  }

main :: IO ()
main = do
  let outDir = "output"
  createDirectoryIfMissing True outDir

  let meshes =
        [ ("saddle.obj", generateSurfaceMesh saddle 20 20)
        ]

  forM_ meshes $ \(filename, mesh) ->
    case exportSurfaceMeshOBJ mesh of
      Left err -> putStrLn $ "❌ Failed to export " ++ filename ++ ": " ++ err
      Right objText -> do
        let outPath = outDir </> filename
        writeFile outPath objText
        putStrLn $ "✅ Exported mesh to: " ++ outPath

  let torusMesh = generateSurfaceMesh sinusoidalTorus 50 30
  case exportSurfaceMeshOBJ torusMesh of
    Left err -> putStrLn $ "❌ Torus export failed: " ++ err
    Right torusObj -> do
      let outPathTorus = outDir </> "sinusoidal_torus.obj"
      writeFile outPathTorus torusObj
      putStrLn $ "✅ Exported sinusoidal torus mesh to: " ++ outPathTorus

  let trefoilMesh = generateSurfaceMesh trefoilKnot 100 40
  case exportSurfaceMeshOBJ trefoilMesh of
    Left err -> putStrLn $ "❌ Trefoil export failed: " ++ err
    Right trefoilObj -> do
      let outPathTrefoil = outDir </> "trefoil.obj"
      writeFile outPathTrefoil trefoilObj
      putStrLn $ "✅ Exported trefoil mesh to: " ++ outPathTrefoil

  let heartMesh = generateSurfaceMesh heartSurface 100 40
  case exportSurfaceMeshOBJ heartMesh of
    Left err -> putStrLn $ "❌ Heart surface export failed: " ++ err
    Right heartObj -> do
      let outPathHeart = outDir </> "heart.obj"
      writeFile outPathHeart heartObj
      putStrLn $ "✅ Exported heart mesh to: " ++ outPathHeart
