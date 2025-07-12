module Main where

-- Geometry kernel logic
import Logic.Vector
import Logic.Matrix
import Logic.Metric
import Logic.Space

-- Geometry objects
import Logic.Geometry.Point
import Logic.Geometry.Line
import Logic.Geometry.LineSegment
-- import Logic.Geometry.Geodesic
-- (and soon: Plane, Curve, etc.)

-- IO/Setup
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "🧮 Project Euclid Core Test"

  files <- listDirectory "."
  let euclidFiles = filter (\f -> takeExtension f == ".euclid") files

  space <- case euclidFiles of
    (f:_) -> do
      putStrLn $ "📄 Found Euclid file: " ++ f
      contents <- readFile f
      case parseSpaceHeader contents of
        Just s  -> do
          putStrLn "✅ Loaded space from file."
          return s
        Nothing -> do
          putStrLn "⚠️ Failed to parse header. Falling back to 3D Euclidean."
          return defaultSpace
    [] -> do
      putStrLn "⚠️ No .euclid file found. Falling back to 3D Euclidean."
      return defaultSpace

  let p1 = pointFromList [0.0, 0.0, 0.0 , 0.0]
      p2 = pointFromList [1.0, 2.0, 2.0, 1.0]
      result = distanceIn space p1 p1 p2

  case result of
    Just d  -> putStrLn $ "📏 Distance between p1 and p2: " ++ show d
    Nothing -> putStrLn "❌ Failed to compute distance."

-- Fallback default space: 3D Euclidean
defaultSpace :: Floating a => Space a
defaultSpace = Space 3 (ConstantMetric [[1,0,0],[0,1,0],[0,0,1]])

-- Simple parser to extract space dim and constant metric from file header
parseSpaceHeader :: String -> Maybe (Space Double)
parseSpaceHeader input = do
  let ls = lines input
      dimLine    = findLinePrefix "space " ls
      metricLine = findLinePrefix "metric constant " ls

  dimStr <- stripPrefix "space " =<< dimLine
  dim <- readMaybe dimStr

  matStr <- stripPrefix "metric constant " =<< metricLine
  matrix <- readMaybe matStr :: Maybe [[Double]]

  return (Space dim (ConstantMetric matrix))

-- Utility: Find first line starting with a prefix
findLinePrefix :: String -> [String] -> Maybe String
findLinePrefix prefix = safeHead . filter (prefix `isPrefixOf`)

-- Utility: Strip prefix if it matches
stripPrefix :: String -> String -> Maybe String
stripPrefix pre str
  | pre `isPrefixOf` str = Just (drop (length pre) str)
  | otherwise            = Nothing

-- Safe head
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x